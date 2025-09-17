(in-package :slp)

;;----------------------------------;;
;; links to helvetica hamnosys font ;;
;;----------------------------------;;

  (setf web-interface::*dispatch-table*
        (append web-interface::*dispatch-table*
                (list (web-interface::create-static-file-dispatcher-and-handler 
                       "/HelveticaNeue-Hamnosys-Roman.ttf" (merge-pathnames
                                        (make-pathname :directory '(:relative "helveticaneue-hamnosys" "roman")
                                        :name "HelveticaNeue-Hamnosys-Roman" :type "ttf")
                                        *requirements-folder*))
                      (web-interface::create-static-file-dispatcher-and-handler 
                       "/HelveticaNeue-Hamnosys-Roman.woff" (merge-pathnames
                                        (make-pathname :directory '(:relative "helveticaneue-hamnosys" "roman")
                                        :name "HelveticaNeue-Hamnosys-Roman" :type "woff")
                                        *requirements-folder*))
                      (web-interface::create-static-file-dispatcher-and-handler 
                       "/HelveticaNeue-Hamnosys-Roman.woff2" (merge-pathnames
                                        (make-pathname :directory '(:relative "helveticaneue-hamnosys" "roman")
                                        :name "HelveticaNeue-Hamnosys-Roman" :type "woff2")
                                        *requirements-folder*))
                      (web-interface::create-static-file-dispatcher-and-handler 
                       "/HelveticaNeue-Hamnosys-Bold.ttf" (merge-pathnames
                                        (make-pathname :directory '(:relative "helveticaneue-hamnosys" "bold")
                                        :name "HelveticaNeue-Hamnosys-Bold" :type "ttf")
                                        *requirements-folder*))
                      (web-interface::create-static-file-dispatcher-and-handler 
                       "/HelveticaNeue-Hamnosys-Bold.woff" (merge-pathnames
                                        (make-pathname :directory '(:relative "helveticaneue-hamnosys" "bold")
                                        :name "HelveticaNeue-Hamnosys-Bold" :type "woff")
                                        *requirements-folder*))
                      (web-interface::create-static-file-dispatcher-and-handler 
                       "/HelveticaNeue-Hamnosys-Bold.woff2" (merge-pathnames
                                        (make-pathname :directory '(:relative "helveticaneue-hamnosys" "bold")
                                        :name "HelveticaNeue-Hamnosys-Bold" :type "woff2")
                                        *requirements-folder*))
                      (web-interface::create-static-file-dispatcher-and-handler 
                       "/courier-hamnosys.ttf" (merge-pathnames
                                        (make-pathname :directory '(:relative "courier-hamnosys")
                                        :name "Courier-HamNoSys-Normal" :type "ttf")
                                        *requirements-folder*))
                      (web-interface::create-static-file-dispatcher-and-handler 
                       "/courier-hamnosys.woff" (merge-pathnames
                                        (make-pathname :directory '(:relative "courier-hamnosys")
                                        :name "Courier-HamNoSys-Normal" :type "woff")
                                        *requirements-folder*))
                      (web-interface::create-static-file-dispatcher-and-handler 
                       "/courier-hamnosys.woff2" (merge-pathnames
                                        (make-pathname :directory '(:relative "courier-hamnosys")
                                        :name "Courier-HamNoSys-Normal" :type "woff2")
                                        *requirements-folder*)))))

 

;;-----------------;;
;; CSS definitions ;;
;;-----------------;;

;; link to the avatar css library
(web-interface::define-css-link 'cwasa.css "https://vhg.cmp.uea.ac.uk/tech/jas/vhg2025/cwa/cwasa.css")


;; define fonts that include hamnosys
(define-css 'helvetica-hamnosys-normal "
@font-face {
    font-family: 'Helvetica Neue Hamnosys';
    src: url('./HelveticaNeue-Hamnosys-Roman.woff2') format('woff2'),
         url('./HelveticaNeue-Hamnosys-Roman.woff') format('woff'),
         url('./HelveticaNeue-Hamnosys-Roman.ttf') format('truetype');
    font-weight: normal;
    font-style: normal;

}")

(define-css 'helvetica-hamnosys-bold "
@font-face {
    font-family: 'Helvetica Neue Hamnosys';
    src: url('./HelveticaNeue-Hamnosys-Bold.woff2') format('woff2'),
         url('./HelveticaNeue-Hamnosys-Bold.woff') format('woff'),
         url('./HelveticaNeue-Hamnosys-Bold.ttf') format('truetype');
    font-weight: bold;
    font-style: bold;

}")

(define-css 'courier-hamnosys-normal "
@font-face {
    font-family: 'Courier Hamnosys';
    src: url('./courier-hamnosys.woff2') format('woff2'),
         url('./courier-hamnosys.woff') format('woff'),
         url('./courier-hamnosys.ttf') format('truetype');
    font-weight: normal;
    font-style: normal;

}")


;; overwriting some of the standard css definitions of the web-interface to switch fonts to ones that include HamNoSys

(define-css 'web-interface::main "
body, td { font-size: 9pt; font-family: Helvetica Neue Hamnosys, Helvetica Neue, Helvetica, Arial;}
body {background-color:#FFFFFF;}
a { color: #000066; text-decoration:none; }
a:hover {text-decoration: underline}
a.button { font-size: 8pt;}
hr { border:0px;color:#777;background-color:#777;height:1px;width:100%;}
")

(define-css 'web-interface::pprint "
div.pprint { margin-top:0px;}
div.pprint * { font-family: Courier HamNoSys, Courier;font-weight:normal;font-size:9pt;line-height:10px;display:inline-block; }
div.pprint span.table { margin-top:0px; margin-bottom:0px; display:inline-table;border-collapse:collapse;}
div.pprint span.table > span { display:table-cell;vertical-align:top; margin-top:0px; margin-bottom:0px; }
")

;; the whole sign table
(define-css 'sign-table  "
.sign-table {width: 80%; fixed-layout:fixed; position: relative; top: -20px;}")

;; empty cells in table
(define-css 'empty "
.empty {background-color: transparent; upper-border: 1px solid black;}
")

;; row header cells
(define-css 'header "
.header {padding-right:5px; font-weight:500; width:25px; text-align: center;}
")

;; row header text
(define-css 'header-text "
.header-text {font-weight: bold; font-color: black;}
")

; a cell with manual information
(define-css 'id-gloss-cell "
.id-gloss-cell {width: 100% ;padding-left: 10px; padding-right: 10px; background: #159A9C; border-radius: 5px; box-shadow: 1px 1px 1px #ddd; text-align: center; overflow: hidden; text-overflow: ellipsis; word-wrap: break-word;}")

; a cell with hamnosys
(define-css 'hamnosys-cell "
.hamnosys-cell {padding-left: 10px; padding-right: 10px; background: transparent; text-align: center; overflow: hidden; text-overflow: ellipsis; word-wrap: break-word; width: fit-content;}
")

;; text representing an articulation
(define-css 'articulation-text "
.articulation-tag {color: white; display: inline-block; font-weight:400px; font-size: 10px;}
")

;; button for playing one sign
(define-css 'play-sigml-button "
.playsigml {color: white; font-size: 10px;font-weight: 400px; margin-left: 2px; border: none; background-color: transparent; overflow: hidden;}
")

(define-css 'h1 "
h1 {background-color:#002333; color:#FFFFFF; margin-top: 2px; padding: 10px; margin-left: 0; margin-right: 5px;}")

(define-css 'h2 "
h2 {background-color:none; color:black; margin-top: 2px; margin-left: 10px; margin-right: 5px;}")

(define-css 'h3 "
h3 {background-color:none; color:black; margin-left: 10px; font-family: Helvetica Neue;}")

;; background of the avatar
(define-css 'divav "
.divAv {
	box-sizing: border-box;
	position: relative; background: #DEEFE7;
	width: 100%; height: 100%;
min-height: 200px;
	margin: 0px; border: 1px solid; padding: 0px;
}")

(define-css 'banner "
.banner {
	background: #B4BEC9;
        padding: 10px;
        margin-left: 0px;
}")


;; add onmouseover action to body of interface to initiate avatar use (not sure if this is the best option)
(wi::define-easy-handler (wi::main-page :uri "/") ()
  (render-xml
   `((html :xmlns "http://www.w3.org/1999/xhtml")
     ((head)
      ((title) "Babel web interface")
      ((link :rel "icon" :href "/favicon.ico" :type "image/png"))
      ,(wi::generate-prologue wi::*ajax-processor*)
      ,@(wi::get-combined-js-library-definitions)
      ,@(wi::get-combined-css-link-definitions)
      ,(wi::get-combined-js-definitions)
      ,(wi::get-combined-css-definitions))
     ((body :onLoad "window.setTimeout(getRequests,500);" :onmouseover "CWASA.init({ambIdle:false,useClientConfig:false});")
      ((div :id "content"))
      ,@(unless wi::*no-reset-button*
                '(((p) ((a :class "button" :href "javascript:ajax_reset();") 
                        "reset"))))))))

;; resetting web-interface with new values
(web-interface::clear-page)