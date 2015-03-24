#lang racket/gui

(require net/http-client net/sendurl net/url net/head xml xml/path racket/include)
(include "parsing.rkt")

;Names of objects
(define project-title "Music4U -- Powered by MusicBrainz")
(define search-bar-title "Search")
(define results-title "Album Lookup")
(define related-title "Related Music Lookup")
(define image-window-title "Image")
(define search-bar-message "Hello, Welcome to Music4U. Please enter the name of an artist you would like to search for.")

;Get the screen size
(define-values (screen-width screen-height) (get-display-size))

;Margin and border sizes of windows
(define my-margin 6)
(define my-border 5)

;Lists of data for gui to display objects
(define image-data '(#f))
(define related-image-data '(#f))
(define selected-artist-data "")
(define selected-album-data "")
(define related-artist-data "")
(define related-album-data "")

;Defines the fixed length of data-set.
(define data-length (length image-data))

;-------------------------------------------
;Definitions of Classes.

;Class used to update the images within the results
;window.
(define netimage%
  (class canvas%
    (super-new)
    (inherit get-dc)
    (init-field [bitmap-pos 0])
    (init-field [display-procedure #f])
    (init-field [clickable? #f])
    ;display full size window of just the image
    (define/override (on-event event)
      (if clickable?
          ;pop up new window with only the image
          (let ([new-event (send event button-up? 'left)])
            (if new-event
                (send (new image-window%
                           [bitmap-pos bitmap-pos])
                      show #t)
                ;else
                #f))
          ;else
          #f
          ))
    ;draw the bitmap image depending on what is within "image-data".
    (define/override (on-paint)
      (let ([my-dc (get-dc)] [pos 0])
        (if (list? image-data)
            (if (equal? (length image-data) data-length)
                (cond ((equal? bitmap-pos 1) (send my-dc draw-bitmap (get-image (first image-data)) pos pos))
                      ((equal? bitmap-pos 2) (send my-dc draw-bitmap (get-image (first related-image-data)) pos pos))
                      (else (send my-dc draw-bitmap (read-bitmap "image-not-found.png") pos pos)))
                ;else
                (send my-dc draw-bitmap (read-bitmap "image-not-found.png") pos pos))
            ;else
            (send my-dc draw-bitmap (read-bitmap "image-not-found.png") pos pos))))))


;Resizable window holding a full image
(define image-window%
  (class object%
    (super-new)
    (init-field [bitmap-pos 0])
    
    (define frame
      (new frame%
           [label image-window-title]
           [border my-border]
           [width 
            (let ([temp-width screen-width])
              500)]
           [height 
            (let ([temp-height screen-width])
              500)]    
           [min-height 200]
           [min-width 200]))
    
    (define netimage
      (new netimage%
           [parent frame]
           [bitmap-pos bitmap-pos]))
    
    (define/public (show bool)
      (send frame show bool))))

;Window that displays loading message
(define loading-window%
  (class object%
    (super-new)
    (init-field [bitmap-pos 0])
    
    (define frame
      (new frame%
           [label "Loading..."]
           [border my-border]
           [width 
            (let ([temp-width screen-width])
              100)]
           [height 
            (let ([temp-height screen-width])
              50)]    
           [min-height 50]
           [min-width 100]))
    
    (define vert-panel
      (new vertical-panel%
           [parent frame]
           [horiz-margin my-margin]
           [vert-margin my-margin]
           [alignment '(center center)]))
    
    (define load-font (make-object font% 14 'default))
    
    (define message
      (new message%
           [parent vert-panel]
           [font load-font]
           [label "Please wait... Your results will show up soon. This may take a minute.\n"]))
    
    (define/public (show bool)
      (send frame show bool))))


;Show related-results in results-window
(define related-window%
  (class object%
    (super-new)
    
    (define frame
      (new frame%
           [label related-title]
           [width 
            (let ([temp-width screen-width])
             (* (quotient temp-width 4) 3))]
           [height 
            (let ([temp-height screen-height])
              (* (quotient temp-height 3) 2))]
           ))
    
    (define header-font (make-object font% 24 'default))
    
    (define vert-panel
          (new vertical-panel%
           [parent frame]
           [horiz-margin my-margin]
           [vert-margin my-margin]
           [min-height 5]
           [alignment '(center center)]))
    
    (define artist-header-panel
      (new horizontal-panel%
           [parent vert-panel]
           [horiz-margin my-margin]
           [vert-margin 2]
           [min-height 10]
           [stretchable-height 5]
           [alignment '(center top)]))
    
    (define artist-panel 
      (new horizontal-panel%
           [parent vert-panel]
           [horiz-margin my-margin]
           [stretchable-height 500]
           [vert-margin 2]
           [alignment '(center bottom)]))
    
    (define album-header-panel
      (new horizontal-panel%
           [parent vert-panel]
           [horiz-margin my-margin]
           [vert-margin 2]
           [min-height 10]
           [stretchable-height 5]
           [alignment '(center top)]))
    
    (define album-panel 
      (new horizontal-panel%
           [parent vert-panel]
           [horiz-margin my-margin]
           [vert-margin 2]
           [stretchable-height 500]
           [alignment '(center bottom)]))
    
    (define artist-title
          (new message%
           [label "Related Artists\n"]
           [font header-font]
           [parent artist-header-panel]))
    
    (define album-title
          (new message%
           [label "Related Albums\n"]
           [font header-font]
           [parent album-header-panel]))
    
    (define image-result
      (new netimage%
           [parent album-panel]
           [min-width 100]
           [stretchable-width 100]
           [stretchable-height 100]
           [clickable? #t]
           [bitmap-pos 2]))
    
    (define message2
          (new message%
           [label "Single Click to display album artwork or Double Click an artist/album to search YouTube.\n"]
           [parent vert-panel]))
    
 
    (define artist-list
      (new list-box%
           [label "Results"]
           [min-height 150]
           [min-width 500]
           [stretchable-height 250]
           [parent artist-panel]
           [callback (lambda (l e)
                       (when (eq? (send e get-event-type) 'list-box-dclick)
                         (define arid-index (send artist-list get-selections))
                         (define arid-res (send artist-list get-string (car arid-index)))
                         (sleep 3)
                         (define related-res (get-selected-artist-data arid-res))
                         (send-url (~a "https://www.youtube.com/results?search_query=" (cadr related-res)))))]
           [choices (list "N/A")]
           [style (list 'single
                        'column-headers)]
           [columns (list "ARID:" "Artist:" "Disambiguation:" "Country:" "Tags:")]
           ))
    
    (send artist-list set-column-width 0 295 295 295)
    (send artist-list set-column-width 1 100 100 125)
    (send artist-list set-column-width 2 125 125 175)
    (send artist-list set-column-width 3 75 75 75)
    (send artist-list set-column-width 4 295 295 305)
    
    (define album-list
      (new list-box%
           [label "Results"]
           [min-height 375]
           [stretchable-height 500]
           [min-width 500]
           [parent album-panel]
           [callback (lambda (l e)
                       (when (eq? (send e get-event-type) 'list-box)
                         (define reid-index (send album-list get-selections))
                         (define reid-res (send album-list get-string (car reid-index)))
                         (set! related-image-data (get-coverart-url reid-res))
                         (send image-result refresh-now))
                       (when (eq? (send e get-event-type) 'list-box-dclick)
                         (define reid-index (send album-list get-selections))
                         (define reid-res (send album-list get-string (car reid-index)))
                         (sleep 2)
                         (define related-res (get-selected-album-data reid-res))
                         (set! related-album-data related-res)
                         (send-url (~a (~a (~a "https://www.youtube.com/results?search_query=" (cadr related-res)) " - ") (car related-res)))))]
           [choices (list "N/A")]
           [style (list 'single
                        'column-headers)]
           [columns (list "REID:" "Album:" "Artist:" "Tags:")]
           ))
    
    (send album-list set-column-width 0 295 295 295)
    (send album-list set-column-width 1 125 125 175)
    (send album-list set-column-width 2 125 125 175)
    (send album-list set-column-width 3 295 295 305)
    
    
    ;Operations on class
    (define/public (show bool)
      (send frame show bool))
    
    (define/public (select1 index)
      (send artist-list select index))
    
    (define/public (select2 index)
      (send album-list select index))
    
    (define/public (set1 c1 c2 c3 c4 c5)
      (send artist-list set c1 c2 c3 c4 c5))
    
    (define/public (set2 c1 c2 c3 c4)
      (send album-list set c1 c2 c3 c4))
    
    (define/public (refresh)
      (send image-result refresh))))

;Show album lookup in results-window
(define results-window%
  (class object%
    (super-new)
    
    (define frame
      (new frame%
           [label results-title]
           [width 
            (let ([temp-width screen-width])
             (* (quotient temp-width 4) 3))]
           [height 
            (let ([temp-height screen-height])
              (* (quotient temp-height 3) 5))]
           ))
    
    (define vert-panel
          (new vertical-panel%
           [parent frame]
           [horiz-margin my-margin]
           [vert-margin my-margin]
           [alignment '(center center)]))
    
    (define result-panel 
      (new horizontal-panel%
           [parent vert-panel]
           [horiz-margin my-margin]
           [vert-margin my-margin]
           [alignment '(center bottom)]))
    
    (define image-result
      (new netimage%
           [parent result-panel]
           [min-width 100]
           [stretchable-width 100]
           [stretchable-height 100]
           [clickable? #t]
           [bitmap-pos 1]))
    
    (define message2
          (new message%
           [label "Single Click to display album artwork or Double Click an album to search YouTube.\n"]
           [parent vert-panel]))
    
    (define results-list
      (new list-box%
           [label "Results"]
           [min-height 95]
           [min-width 425]
           [parent result-panel]
           [callback (lambda (l e)
                       (when (eq? (send e get-event-type) 'list-box)
                         (define reid-index (send results-list get-selections))
                         (define reid-res (send results-list get-string (car reid-index)))
                         (set! image-data (get-coverart-url reid-res))
                         (send image-result refresh-now))
                       (when (eq? (send e get-event-type) 'list-box-dclick)
                         (define reid-index (send results-list get-selections))
                         (define reid-res (send results-list get-string (car reid-index)))
                         (sleep 2)
                         (define selected-res (get-selected-album-data reid-res))
                         (send-url (~a (~a (~a "https://www.youtube.com/results?search_query=" (cadr selected-res)) " - ") (car selected-res)))))]
           [choices (list "N/A")]
           [style (list 'single
                        'column-headers)]
           [columns (list "REID:" "Album:" "Artist:" "Tags:")]
           ))
    
    (send results-list set-column-width 0 295 295 295)
    (send results-list set-column-width 1 125 125 175)
    (send results-list set-column-width 2 125 125 175)
    (send results-list set-column-width 3 295 295 305)
    
    (define related-button
      (new button%
           [label "See Related Music"]
           [parent vert-panel]
           [callback (lambda (button event)
                       (define reid-index (send results-list get-selections))
                       (define reid-res (send results-list get-string (car reid-index)))
                       (send loading show #t)
                       (sleep 1)
                       (define selected-artist-res (get-selected-artist-data (car selected-artist-data)))
                       (set! related-artist-data selected-artist-res)
                       (sleep 1)
                       (define selected-album-res (get-selected-album-data reid-res))
                       (set! related-album-data selected-album-res)
                       (sleep 5)
                       (define related-artist-res (get-related-artists (caddr related-artist-data)))
                       (send related set1 
                             (first related-artist-res) 
                             (second related-artist-res) 
                             (third related-artist-res)
                             (fourth related-artist-res)
                             (list-box-tags (last related-artist-res)))
                       (sleep 8)
                       (define related-album-res (get-related-albums (caddr related-album-data)))
                       (send related set2 
                             (car related-album-res) 
                             (cadr related-album-res) 
                             (caddr related-album-res)  
                             (list-box-tags (last related-album-res)))
                       (send related show #t)
                       (set! related-image-data (get-coverart-url (car (car related-album-res))))
                       (send related refresh)
                       (send loading show #f)
                       (send related select1 0)
                       (send related select2 0)
                       (sleep 1)
                       )]))
    
(define related (new related-window%))
(define loading (new loading-window%))
    ;Operations on class
    (define/public (show bool)
      (send frame show bool))
    
    (define/public (select index)
      (send results-list select index))
    
    (define/public (set c1 c2 c3 c4)
      (send results-list set c1 c2 c3 c4))
    
    (define/public (refresh)
      (send image-result refresh))))


;First object the user sees. Primary form of user input.
(define search-window%
  (class object%
    (super-new)
    
    (define frame
      (new frame%
           [label project-title]
           [style '(no-resize-border)]
           [width 1000]
           [height 500]))
    
    (define header-font (make-object font% 14 'default))
    
    (define vert-panel
      (new vertical-panel%
           [parent frame]
           [horiz-margin my-margin]
           [vert-margin my-margin]
           [alignment '(center center)]))
    
    (define message
      (new message%
           [label search-bar-message]
           [font header-font]
           [parent vert-panel]))
    
     (define result-panel 
      (new horizontal-panel%
           [parent vert-panel]
           [horiz-margin my-margin]
           [vert-margin my-margin]
           [alignment '(center bottom)]))
    
    (define message2
      (new message%
           [label "Double Click an item in the list to do an album lookup.\n"]
           [parent vert-panel]))
    
    (define results-list
      (new list-box%
           [label "Results"]
           [min-height 95]
           [parent result-panel]
           [callback (lambda (l e)
                       (when (eq? (send e get-event-type) 'list-box-dclick)
                         (define search-query (send text-box get-value))
                         (define arid-index (send results-list get-selections))
                         (define arid-res (send results-list get-string (car arid-index)))
                         (sleep .95)
                         (set! selected-artist-data (get-selected-artist-data arid-res))
                         (define album-res (get-album-data arid-res "arid"))
                         (send results set 
                               (car album-res) 
                               (cadr album-res) 
                               (caddr album-res)  
                               (list-box-tags (last album-res)))
                         (send results show #t)
                         (send results refresh)
                         (send results select 0)
                         (define reid-index (send results-list get-selections))
                         (define reid-res (send results-list get-string (car reid-index)))
                         (set! image-data (get-coverart-url (car (car album-res))))))]  
           [choices (list "N/A")]
           [style (list 'single
                        'column-headers)]
           [columns (list "ARID:" "Artist:" "Disambiguation:" "Country:" "Tags:")]
           ))
    
    (send results-list set-column-width 0 295 295 295)
    (send results-list set-column-width 1 100 100 125)
    (send results-list set-column-width 2 125 125 175)
    (send results-list set-column-width 3 75 75 75)
    (send results-list set-column-width 4 295 295 305)
    
    (send message2 show #f)
    (send results-list show #f)
    
    (define text-panel 
      (new horizontal-panel%
           [parent vert-panel]
           [horiz-margin my-margin]
           [vert-margin my-margin]
           [alignment '(center bottom)]))
    
    (define text-box 
      (new text-field%
           [label search-bar-title]
           [parent text-panel]))
    
    (define search-button
      (new button%
           [label "Submit"]
           [parent text-panel]
           [callback (lambda (button event)
                       (define search-query (send text-box get-value))
                       (sleep 1)
                       (define artist-res (artist-search-query search-query))
                       (send results-list set 
                             (car artist-res) 
                             (cadr artist-res) 
                             (caddr artist-res)  
                             (cadddr artist-res) 
                             (list-box-tags (last artist-res)))
                       (send results-list select 0)
                       (send message2 show #t)
                       (send results-list show #t)
                       (sleep 1)
                       )]))
    
    (define results (new results-window%))
    
    ;operations on class
    (define/public (show bool)
      (send frame show bool))
    
    (define/public (set c1 c2 c3 c4)
      (send results set c1 c2 c3 c4))
    
    (define/public (select index)
      (send results select index))
    
    (send results show #f)))

;End of class definitions
;--------------------------------------------------
;Class operations

;convert the image at the given url into a bitmap.
(define (get-image image-path)
  (if (string? image-path)
      (make-object bitmap% (get-pure-port
                            (string->url image-path)))
      (read-bitmap "image-not-found.png")))

;-----------------------------

(define search-window (new search-window%))

(send search-window show #t)