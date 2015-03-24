;;--------------------------------------------------------------
;;-              LIST DATA MANIPULATION PROCEDURES             -
;;--------------------------------------------------------------

;;--------------------------------------------------------------
;;                        parse-data 
;;             Used to manually parse certain data
;;--------------------------------------------------------------
;;                         ARGS: 
;; x- List of identifiers (arids, rgids, reids, etc...)
;; path -- se/path XML attribute structure.
;; url -- Query url needed in order to parse data
;; type -- Condition for parsing 'tag or other data.
;; mode -- 'multiple -- Given list of identifiers or 
;; 'single -- given a single identifier.
;;--------------------------------------------------------------

(define (parse-data x path url type mode)
    (if (null? x) '()
      (let*
      (
       (mb-artist-url (if (equal? mode 'single)
                                  (string->url 
                                   (~a url x))
                                  (string->url
                                   (~a url (car x)))))
       (mb-artist-port (get-pure-port mb-artist-url))
       (mb-artist-response (port->string mb-artist-port))
       (mb-artist-data (xml->xexpr
                           (document-element
                            (read-xml/document (open-input-string mb-artist-response)))))
       (artist-query-tags (se-path*/list '(tag-list tag name) mb-artist-data))
       (artist-query-data (se-path* path mb-artist-data)))
        (close-input-port mb-artist-port)
        (sleep 0.05)
        (if (equal? type 'tag)
            (cons (if(null? artist-query-tags) (list "N/A") artist-query-tags) (parse-data (if (equal? mode 'single) x (cdr x)) path url type mode))
            (if (false? artist-query-data) 
                (cons "N/A" (parse-data (if (equal? mode 'single) x (cdr x)) path url type mode))
                (cons artist-query-data (parse-data (if (equal? mode 'single) x (cdr x)) path url type mode)))))))

;;--------------------------------------------------------------
;;                      check-length                            
;;    Checks the length of lists of returned data and applies 
;;    proper procedure whether it be a default se/path*/list
;;    or a manual parse-data pass.
;;--------------------------------------------------------------
;;                         ARGS: 
;; data -- Procedure with results of read-xml
;; id -- List of MBIDS to check the length against.
;; path -- se/path XML attribute structure.
;; url -- Query url needed in order to parse data
;; type -- Condition for parsing 'tag or other data.
;; mode -- 'multiple -- Given list of identifiers or 
;; 'single -- given a single identifier.
;;--------------------------------------------------------------

(define (check-length data id path url type mode)
  (cond ((=(- (length (se-path*/list path data)) (length id))  (- 0 (length id))) (make-list (length id) "N/A"))
      ((= (length (se-path*/list path data)) (length id)) (se-path*/list path data))
      (else (parse-data id path url type mode))))
;    (if (= (length (se-path*/list path data)) (length id))
;      (se-path*/list path data)
;      (parse-data id path url type mode)))

;;--------------------------------------------------------------
;;                         tree-manip
;;      This procedure applies an operation and accumlator to 
;;      a nested list (tree) that creates a new list.
;;--------------------------------------------------------------
;;                            ARGS:
;; tree - Nested list
;; init - Initial Result
;; leaf - operation
;; accum - Accumulator
;;--------------------------------------------------------------

(define (tree-manip tree init leaf accum) 
  (cond ((null? tree) init) 
        ((not (pair? tree)) (leaf tree)) 
        (else (accum  
               (tree-manip (car tree) init leaf accum) 
               (tree-manip (cdr tree) init leaf accum)))))

;;--------------------------------------------------------------
;;                       list-box-tags
;;    Formats the nested list of tags into a single list that
;;    can be handled by a list-box GUI widget.
;;--------------------------------------------------------------
;;                           ARGS:
;; lst - Nested list of tag data.
;;--------------------------------------------------------------

(define (list-box-tags lst)
    (if (null? lst) '()
        (let ((string (string-join (car lst) ", ")))
        (if (> (string-length string) 198)
            (cons (substring string 0 197) (list-box-tags (cdr lst)))
            (cons string (list-box-tags (cdr lst)))))))

;;--------------------------------------------------------------
;;-                         amp-append
;;--------------------------------------------------------------
;;- Used to append the ampersand from an se/path string which 
;;- causes display issues.
;;--------------------------------------------------------------
;;                             ARG:
;; list -- flat list
;;--------------------------------------------------------------

(define (amp-append list)
  (foldr (lambda (v l)
         (cond ((equal? l '()) (cons v l))
               ((equal? v "&") (cons (string-append v (car l)) (cdr l)))
               ((equal? (string-ref (car l) 0) #\&) (cons (string-append v (car l)) (cdr l)))
               (else (cons v l))))
       '() list))

;;--------------------------------------------------------------
;;                         limit-tags                          -
;; Limits the amount of tags in order to do a relevant related
;; search.
;;--------------------------------------------------------------
;;                            ARGS:
;; lst -- list of tags.
;; result -- final result
;; count -- Counter.
;; limit -- # of tags to limit to.
;;--------------------------------------------------------------

(define (limit-tags lst result count limit)
  (if (>= count limit) result
      (if (null? lst) result
          (limit-tags (cdr lst) (append result (list (car lst))) (+ count 1) limit))))

;;--------------------------------------------------------------
;;                     related-artist-func                     -
;;       Builds the query string from tags for the url         -                
;;--------------------------------------------------------------
;;                            ARGS:
;; lst -- list of tags.
;; result -- final result
;;--------------------------------------------------------------

(define (related-artist-func lst result)
  (if (null? lst) result
  (related-artist-func (cdr lst) (~a result (~a(~a "tag:" (if (equal? (car lst) "N/A") "music" (car lst)) (if (null? (cdr lst)) "" " AND ")))))))

;;--------------------------------------------------------------
;;            END OF LIST DATA MANIPULATION PROCEDURES         -
;;--------------------------------------------------------------


;;--------------------------------------------------------------
;;-                    Artist Search Query                     -
;;--------------------------------------------------------------
;;-                    artist-search-query                     -
;;--------------------------------------------------------------
;;-  Performs an artist search query and returns top artists   -
;;--------------------------------------------------------------
;;                              ARG:
;; search-query -- query submitted from callback.              
;;--------------------------------------------------------------


(define (artist-search-query search-query)
  (define musicbrainz-artist-query "http://musicbrainz.org/ws/2/artist/?limit=5&query=artist:")
  (define mb-artist-full-url (string->url (~a musicbrainz-artist-query search-query)))
  (define mb-artist-query-port (get-pure-port mb-artist-full-url))
  (define mb-artist-response (port->string mb-artist-query-port))
  (close-input-port mb-artist-query-port)
  
  (define artist-query-data (xml->xexpr
                             (document-element
                              (read-xml/document (open-input-string mb-artist-response)))))
  
  (define artist-query-mbid (se-path*/list '(artist #:id) artist-query-data))
  
  
  
  (define artist-query-name
    ;(amp-append (check-length artist-query-data artist-query-mbid '(artist name) "http://musicbrainz.org/ws/2/artist/?limit=5&query=arid:" 'name 'multiple)))
    (amp-append (se-path*/list '(artist name) artist-query-data)))
  
  (define artist-query-disambiguation (check-length artist-query-data artist-query-mbid '(artist disambiguation) "http://musicbrainz.org/ws/2/artist/?query=arid:" 'dis 'multiple))
  (define artist-query-country (check-length artist-query-data artist-query-mbid '(artist country) "http://musicbrainz.org/ws/2/artist/?query=arid:" 'country 'multiple))
  (define artist-query-tags (parse-data artist-query-mbid 
                                                    '(tag-list tag name) 
                                                    "http://musicbrainz.org/ws/2/artist/?query=arid:" 
                                                    'tag 'multiple))
  
  (list 
   artist-query-mbid 
   artist-query-name 
   artist-query-disambiguation 
   artist-query-country 
   (tree-manip artist-query-tags 
               '() (lambda (x) (regexp-replace* #rx" " x "-")) cons)))

;;--------------------------------------------------------------
;;                     Selected Artist Inquiry                 -
;;--------------------------------------------------------------
;;  Gets tags for selected artist within search query list box.
;;--------------------------------------------------------------
;;                              ARGS:
;; arid- MusicBrainz artist id.
;;--------------------------------------------------------------

(define (get-selected-artist-data arid)
  (define mb-selected-artist-url "http://musicbrainz.org/ws/2/artist/?query=arid:")
  (define mb-selected-artist-full-url (string->url (~a mb-selected-artist-url arid)))
  (define mb-selected-artist-query-port (get-pure-port mb-selected-artist-full-url))
  (define mb-selected-artist-response (port->string mb-selected-artist-query-port))
  (close-input-port mb-selected-artist-query-port)
  
  (define selected-artist-data (xml->xexpr
                                (document-element
                                 (read-xml/document (open-input-string mb-selected-artist-response)))))  
  
  (define selected-artist-mbid (se-path* '(artist #:id) selected-artist-data))

  (define selected-artist-name
        (se-path* '(artist name) selected-artist-data))
  
  (define selected-artist-tags
    (let ((tags (se-path*/list '(tag-list tag name) selected-artist-data)))
    (if (null? tags) (list "N/A") tags)))
  
  (define limited-artist-tags (limit-tags selected-artist-tags '() 0 3))
  
  (list selected-artist-mbid selected-artist-name (tree-manip limited-artist-tags 
               '() (lambda (x) (regexp-replace* #rx" " x "-")) cons)))

;;--------------------------------------------------------------
;;                     Selected Album Inquiry                 -
;;--------------------------------------------------------------
;;  Gets name, artist and tags for selected artist within search 
;;  query list box.
;;--------------------------------------------------------------
;;                              ARGS:
;; reid- MusicBrainz release id.
;;--------------------------------------------------------------

(define (get-selected-album-data reid)
  (define mb-selected-album-url "http://musicbrainz.org/ws/2/release-group/?query=reid:")
  (define mb-selected-album-full-url (string->url (~a mb-selected-album-url reid)))
  (define mb-selected-album-query-port (get-pure-port mb-selected-album-full-url))
  (define mb-selected-album-response (port->string mb-selected-album-query-port))
  (close-input-port mb-selected-album-query-port)
  
  (define selected-album-data (xml->xexpr
                                (document-element
                                 (read-xml/document (open-input-string mb-selected-album-response)))))  
  
  
  (define selected-album-name (se-path* '(release-group title) selected-album-data))
  (define selected-album-artist (se-path* '(artist-credit name-credit artist name) selected-album-data))
  (define selected-album-tags
    (let ((tags (se-path*/list '(tag-list tag name) selected-album-data)))
    (if (null? tags) (list "N/A") tags)))
  
  (define limited-album-tags (limit-tags selected-album-tags '() 0 3))
  
  (list selected-album-name selected-album-artist 
        (tree-manip limited-album-tags 
               '() (lambda (x) (regexp-replace* #rx" " x "-")) cons)))

;;--------------------------------------------------------------
;;-                        Album Lookup                        -
;;--------------------------------------------------------------
;;-                       get-album-data                       -
;;--------------------------------------------------------------
;;-   Performs an album search query and returns top albums    -
;;--------------------------------------------------------------
;;                              ARG:
;; mbid -- MusicBrainz unique artist identifier.
;; idtype -- mbid type
;;--------------------------------------------------------------
  

(define (get-album-data mbid idtype)
  (define musicbrainz-album-lookup "http://musicbrainz.org/ws/2/release-group/?limit=5&query=")
  (define mb-album-full-url (string->url (~a(~a(~a (~a musicbrainz-album-lookup idtype) ":") mbid) " AND type:album")))
  (define mb-album-lookup-port (get-pure-port mb-album-full-url))
  (define mb-album-lookup-response (port->string mb-album-lookup-port))
  (close-input-port mb-album-lookup-port)
  
  (define album-lookup-data (xml->xexpr
                             (document-element
                              (read-xml/document (open-input-string mb-album-lookup-response)))))
  
  (define album-mbid (se-path*/list '(release-group #:id) album-lookup-data))
  (define album-title (amp-append (se-path*/list '(release-group title) album-lookup-data)))
  (define album-artist (amp-append (se-path*/list '(artist-credit name-credit artist name) album-lookup-data)))
  (define album-tag (parse-data album-mbid '(tag-list tag name) "http://musicbrainz.org/ws/2/release-group/?query=rgid:" 'tag 'multiple))
  (define album-release-mbid (parse-data album-mbid '(release-list release #:id) "http://musicbrainz.org/ws/2/release-group/?query=rgid:" 'releases 'multiple))
  
  (list album-release-mbid album-title album-artist  
                      (tree-manip album-tag '() (lambda (x) (regexp-replace* #rx" " x "-")) cons)))

;;----------------------------------------------------------------------
;;-                      Related Artist Search                         -
;;----------------------------------------------------------------------
;;-  Performs a related artist search on the tags given as an argument. -
;;----------------------------------------------------------------------
;;-                               ARGS:                                -
;;- tags- list of tags
;;----------------------------------------------------------------------

(define (get-related-artists tags)
  (define mb-related-artist-query
    (string->url (~a "http://musicbrainz.org/ws/2/artist/?limit=5&query=" 
                     (related-artist-func tags "" ))))
  
  (define mb-related-lookup-port (get-pure-port mb-related-artist-query))
  (define mb-related-lookup-response (port->string mb-related-lookup-port))
  (close-input-port mb-related-lookup-port)
  
  (define artist-related-lookup-data (xml->xexpr
                                      (document-element
                                       (read-xml/document (open-input-string mb-related-lookup-response)))))

  (define related-artist-query-mbid (se-path*/list '(artist #:id) artist-related-lookup-data))

  (define related-artist-query-name
        (amp-append (se-path*/list '(artist name) artist-related-lookup-data)))
  
  (define related-artist-query-disambiguation (check-length artist-related-lookup-data related-artist-query-mbid '(artist disambiguation) "http://musicbrainz.org/ws/2/artist/?query=arid:" 'dis 'multiple))
  
  (define related-artist-query-country (check-length artist-related-lookup-data related-artist-query-mbid '(artist country) "http://musicbrainz.org/ws/2/artist/?query=arid:" 'country 'multiple))

  (define related-artist-query-tags (tree-manip (parse-data related-artist-query-mbid 
                                                  '(tag-list tag name) 
                                                  "http://musicbrainz.org/ws/2/artist/?query=arid:" 
                                                  'tag 'multiple) '() (lambda (x) (regexp-replace* #rx" " x "-")) cons)) 

  (list related-artist-query-mbid related-artist-query-name related-artist-query-disambiguation related-artist-query-country related-artist-query-tags))

;;----------------------------------------------------------------------
;;-                      Related Albums Search                         -
;;----------------------------------------------------------------------
;;-  Performs a related album search on the tags given as an argument. -
;;----------------------------------------------------------------------
;;-                               ARGS:                                -
;;- tags- list of tags
;;----------------------------------------------------------------------


(define (get-related-albums tags)

(define mb-related-album-lookup (string->url (~a(~a "http://musicbrainz.org/ws/2/release-group/?limit=5&query=" 
                       (related-artist-func tags "" ))" AND primarytype:Album AND status:Official")))

(define mb-related-album-lookup-port (get-pure-port mb-related-album-lookup))
(define mb-related-album-lookup-response (port->string mb-related-album-lookup-port))
(close-input-port mb-related-album-lookup-port)

(define album-related-lookup-data (xml->xexpr
             (document-element
             (read-xml/document (open-input-string mb-related-album-lookup-response)))))


(define related-album-rgid (se-path*/list '(release-group #:id) album-related-lookup-data))
  
(define related-album-title (amp-append(se-path*/list '(release-group title) album-related-lookup-data)))
  
(define related-album-artist (amp-append (check-length album-related-lookup-data related-album-rgid '(release-group artist-credit name-credit artist name) "http://musicbrainz.org/ws/2/release-group/?query=rgid:" 'name 'multiple)))
 
(define related-album-tag (parse-data related-album-rgid '(tag-list tag name) "http://musicbrainz.org/ws/2/release-group/?query=rgid:" 'tag 'multiple))

(define related-album-reid (parse-data related-album-rgid '(release-list release #:id) "http://musicbrainz.org/ws/2/release-group/?query=rgid:" 'releases 'multiple))

(list related-album-reid related-album-title related-album-artist related-album-tag))

;;----------------------------------------------------------------------
;;                            Cover Art
;; ---------------------------------------------------------------------
;;-           Get Album Artwork url for specified reid                 -
;;----------------------------------------------------------------------
;;                               ARG:
;; reid -- MusicBrainz release id (reid)
;;----------------------------------------------------------------------

(define (get-coverart-url reid)
  (define caa-art-lookup "http://coverartarchive.org/release/")
  (define caa-full-url (string->url (~a (~a caa-art-lookup reid) "/front")))
  (define caa-art-lookup-port (get-impure-port caa-full-url))
  (define caa-art-lookup-response (port->string caa-art-lookup-port))
  
  (close-input-port caa-art-lookup-port)
  
  (if (false? (extract-field "Location" caa-art-lookup-response)) (list #f)
      (let* ((redirected-url (string->url (extract-field "Location" caa-art-lookup-response)))
             (caa-art-lookup-port2 (get-impure-port redirected-url))
             (caa-art-lookup-response2 (port->string caa-art-lookup-port2))
             (x (extract-field "Location" caa-art-lookup-response2)))
        (close-input-port caa-art-lookup-port)
         (list (regexp-replace* #rx".jpg" x "_thumb500.jpg"))))) 
