(defpackage :netersys
  (:use :cl :cl-serve :cl-who)
  (:export :start-netersys
	   :stop-netersys))

(in-package :netersys)

(defvar *host-name* "netersys.com")
(defvar *prj-dir* (asdf:component-pathname
		   (asdf:find-system :netersys)))
(setf cl-serve:*project-dir* *prj-dir*)
(defvar *src-dir* (merge-pathnames #P"src/" *prj-dir*))
(defvar *stic-dir* (merge-pathnames #P"static/" *prj-dir*))
(setf cl-serve:*static-dir* *stic-dir*)
(defvar *imgs-dir* (merge-pathnames #P"imgs/" *stic-dir*))
(defvar *css-dir* (merge-pathnames #P"css/" *stic-dir*))
(defvar *js-dir* (merge-pathnames #P"js/" *stic-dir*))

(defvar *dir-match* `((img . ,*imgs-dir*)
		      (js . ,*js-dir*)
		      (css . ,*css-dir*)))

(defparameter *cert* (merge-pathnames #P"certs/fullchain.pem" *static-dir*))
(defparameter *privkey* (merge-pathnames #P"certs/privkey.pem" *static-dir*))

;;; Start a socket server
(defun start-netersys (&key (port 80) (cert *cert*) (privkey *privkey*))
  (start :port port :cert cert :privkey privkey))

(defun stop-netersys ()
  (stop))

(defun link (&optional (where 'loc) (sublink nil) (ac nil))
  (concatenate 'string
	       (if (eq where 'loc)
		   (concatenate 'string *host-name*
				(if sublink
				    (concatenate 'string "/" sublink
						 (if ac
						     (concatenate 'string "#" ac)
						     ""))
				    ""))
		   (if sublink
		       sublink
		       (error "A valid URL cannot be obtained from the given arguments")))))

(defun path (where filename)
  (let ((root (cdr (assoc where *dir-match*))))
    (print root)
    (merge-pathnames filename root)))

(defmacro img (filename)
  `(path 'img ,filename))
(defmacro css (filename)
  `(path 'css ,filename))
(defmacro js (filename)
  `(path 'js ,filename))
  
(defhandler ("not-found" :status 'not-found)
  (with-html-output (*standard-output*)
    (:html
     (:head
      (:title "Page not found | netersys"))
     (:body
      (:p "The page you are requesting does not exist yet!")
      (:hr)
      (:i "cl-serve 1.0")))))

(defmacro generic-page (title &body body)
  `(with-html-output (*standard-output*)
     ((:html :lang "fr")
      (:head
       (:title ,title)
       (:meta :charset "utf-8")
       (:meta :name "viewport" :content "width=device-width, initial-scale=1, shrink-to-fit=no")
       (:meta :name "author" :content "Arnold NGORAN")
       (:meta :name "description" :content "Open energy data platform and energy management system")
       (:meta :name "keywords" :content "energy management system, optimization, optimal control, energy,
renewable energy")
       (:link :rel "shortcut icon" :href "imgs/favicon.png")
       ;;(:link :rel "stylesheet" :href "css/linearicons.css")
       ;;(:link :rel "stylesheet" :href "css/font-awesome.min.css")
       (:link :rel "stylesheet" :href "https://code.jquery.com/ui/1.12.1/themes/base/jquery-ui.css")
       (:link :rel "stylesheet" :href "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"
	      :integrity "sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T"
	      :crossorigin "anonymous")
       (:link :rel "stylesheet" :href "css/svgicons.css")       
       (:link :rel "stylesheet" :href "css/main.css"))
      (:body       
       	((:div :class "sticky-top social-and-lang row align-items-center justify-content-end d-flex")
	 ((:a :href "http://facebook.com/netersys")
	  ((:svg :class "svg-icon" :viewBox "0 0 20 20")
	   (:path :class "facebook" :fill "none" :d "M11.344,5.71c0-0.73,0.074-1.122,1.199-1.122h1.502V1.871h-2.404c-2.886,0-3.903,1.36-3.903,3.646v1.765h-1.8V10h1.8v8.128h3.601V10h2.403l0.32-2.718h-2.724L11.344,5.71z")))
	 ((:a :href "http://twitter.com/netersys") ((:svg :class "svg-icon" :viewBox "0 0 20 20")
	   (:path :class "twitter" :fill "none" :d "M18.258,3.266c-0.693,0.405-1.46,0.698-2.277,0.857c-0.653-0.686-1.586-1.115-2.618-1.115c-1.98,0-3.586,1.581-3.586,3.53c0,0.276,0.031,0.545,0.092,0.805C6.888,7.195,4.245,5.79,2.476,3.654C2.167,4.176,1.99,4.781,1.99,5.429c0,1.224,0.633,2.305,1.596,2.938C2.999,8.349,2.445,8.19,1.961,7.925C1.96,7.94,1.96,7.954,1.96,7.97c0,1.71,1.237,3.138,2.877,3.462c-0.301,0.08-0.617,0.123-0.945,0.123c-0.23,0-0.456-0.021-0.674-0.062c0.456,1.402,1.781,2.422,3.35,2.451c-1.228,0.947-2.773,1.512-4.454,1.512c-0.291,0-0.575-0.016-0.855-0.049c1.588,1,3.473,1.586,5.498,1.586c6.598,0,10.205-5.379,10.205-10.045c0-0.153-0.003-0.305-0.01-0.456c0.7-0.499,1.308-1.12,1.789-1.827c-0.644,0.28-1.334,0.469-2.06,0.555C17.422,4.782,17.99,4.091,18.258,3.266")))
	 ((:a :href "http://youtube.com/netersys") ((:svg :class "svg-icon" :viewBox "0 0 24 24")
						    (:path :class "youtube" :fill "none" :d "M19.615 3.184c-3.604-.246-11.631-.245-15.23 0-3.897.266-4.356 2.62-4.385 8.816.029 6.185.484 8.549 4.385 8.816 3.6.245 11.626.246 15.23 0 3.897-.266 4.356-2.62 4.385-8.816-.029-6.185-.484-8.549-4.385-8.816zm-10.615 12.816v-8l8 3.993-8 4.007z"))))
	((:nav :class "navbar sticky-top navbar-expand-lg navbar-light bg-light")
	 ((:a :id "logo" :class "navbar-brand" :href "/")
	  (:img :src "imgs/logo-netersys.png" :alt "logo-netersys"))
	 ((:button :class "navbar-toggler collapsed" :type "button" :data-toggle "collapse"
		   :data-target "#navbarTogglerDemo02" :aria-controls "navbarTogglerDemo01"
		   :aria-expanded "false" :aria-label "Toggle navigation")
	  (:span :class "navbar-toggler-icon"))
	 ((:div :class "collapse navbar-collapse" :id "navbarTogglerDemo02")
	  ((:ul :class "navbar-nav mr-auto mt-2 mt-lg-0")
	   (:li :class "nav-item" ((:a :class "nav-link" :href "/") "Accueil"))
	   (:li :class "nav-item" ((:a :class "nav-link" :href "/produits") "Produits"))
	   (:li :class "nav-item" ((:a :class "nav-link" :href "/projets") "Projets"))
	   (:li :class "nav-item" ((:a :class "nav-link" :href "/forum") "Forum"))
	   (:li :class "nav-item" ((:a :class "nav-link" :href "/about-us")
				   ((:span :class "to-upper") "à") " propos"))
	   (:li :class "nav-item" ((:a :class "nav-link" :href "/contact-us") "Contact")))
	  ((:form :class "form-inline my-2 my-lg-0")
	   (:input :class "form-control mr-sm-2" :type "search" :placeholder "Recherche")
	   ((:button :class "btn btn-outline-success my-2 my-sm-0" :type "submit") "OK"))))
       ,@body
       (:footer
	((:div :class "main-content footer-main")
	 ((:div :id "footer-menu"))
	 ((:div :id "copyright")
	  "Copyright &copy; 2019 " "netersys" ". Tous droits réservés.")))
       ;; Script area       
       (:script :src "https://code.jquery.com/jquery-3.3.1.slim.min.js"
       	:integrity "sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo"
       	:crossorigin "anonymous")
       ;;(:script :type "text/javascript" :src "https://code.jquery.com/jquery-3.4.1.min.js")
       (:script :type "text/javascript" :src "https://code.jquery.com/ui/1.12.1/jquery-ui.min.js")
       (:script :src "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.7/umd/popper.min.js"
		:integrity "sha384-UO2eT0CpHqdSJQ6hJty5KVphtPhzWj9WO1clHTMGa3JDZwrnQq4sF86dIHNDz0W1"
		:crossorigin "anonymous")
       (:script :src "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js"
		:integrity "sha384-JjSmVgyd0p3pXB1rRibZUAYoIIy6OrQ6VrjIEaFf/nJGzIxFDsf4x0xIM+B07jRM"
		:crossorigin "anonymous")
       ;;(:script :type "text/javascript" :src "https://code.jquery.com/jquery-3.4.1.min.js")
       ;;(:script :type "text/javascript" :src "https://code.jquery.com/ui/1.12.1/jquery-ui.min.js")
       ;;(:script :src "https://maps.googleapis.com/maps/api/js?key AIzaSyBhOdIF3Y9382fqJYt5I_sswSrEw5eihAA")
       ))))

(defhandler ("")
  (generic-page "Accueil | netersys"
    (:main
     ((:section :class "banner-area relative home-banner" :id "home")
      (:div :class "overlay overlay-bg")
      ((:div :class "container")
       ((:div :class "row d-flex align-items-center justify-content-center")
	((:div :class "about-content blog-header-content col-lg-12")
	 ((:h1 :class "text-white") "Build a system on top of nature... netersys !")
	 ((:a :href "/produits" :class "primary-btn") "Voir plus")))))
     ((:div :class "main-content")
      (:section
       (:h1 "Quoi de neuf ?")
       ((:p) "Pour plus d'infos sur netersys, " ((:a :href "/about-us#netersys") "cliquez ici."))
       ((:p) "Prenez connaissance avec nos membres, " ((:a :href "/about-us#members") "ici.")))
      (:section
       (:h1 "Nos stats"))))))

(defhandler ("about-us")
  (generic-page "A propos de nous | netersys"
    (:main :class "main-content main-without-banner"
     ((:section :id "netersys")
      ((:h1) "Sur netersys")
      ((:p) "Netersys ..."))
     ((:section :id "members")
       (:h1 "Sur nos membres")
       ((:div :class "card-columns")
	((:div :class "card")
	 (:img :src "/imgs/arnold.jpg" :class "card-img-top" :alt "...")
	 ((:div :class "card-body")
	  ((:h5 :class "card-title") "Arnold N'GORAN")
	  ((:p :class "card-text") "28 ans, diplômé Ingénieur de conception en génie énergétique de l'Institut National Polytechnique Félix HOUPHOUËT-BOIGNY (INPHB, Côte d'Ivoire), Master ès Science et Technologie des énergies renouvelables de l'Ecole polytechnique (Paris) et bientôt Docteur en Contrôle, Optimisation et Prospective de MINES ParisTech." (:br) "Je totalise 4 ans d'expérience dans l'optimisation des systèmes énergétiques, principalement des microgrids à énergies renouvelables. Je souhaite mettre mon expérience au service de mon pays, la Côte d'Ivoire, et de toute l'Afrique." (:br) "Je suis un grand fan de programmation Lisp et d'open source :)")
	  ((:a :href "https://www.linkedin.com/in/arnold-n-goran-90b35088/" :class "btn btn-primary") "Savoir plus")))
	((:div :class "card")
	 (:img :src "/imgs/yves.jpg" :class "card-img-top" :alt "...")
	 ((:div :class "card-body")
	  ((:h5 :class "card-title") "Yves SAHI")
	  ((:p :class "card-text") "Né le 13/08/1989 à GOUEKANGOUINE, j'ai eu un parcours exceptionnel au Lycée Moderne de Guiglo où j'ai obtenu le BAC C. J'intègre alors l'Institut National Polytechnique Félix HOUPHOUËT-BOIGNY (INPHB, Côte d'Ivoire) où, après 2 années de classes préparatoires MP/MPSI et 3 années de cycle Ingénieur, j'obtiens le Diplôme d'Etat d'Ingénieur de conception en Génie Energétique." (:br)
"Ensuite, je fais un passage marqué dans les secteurs de l'énergie, du BTP et du pétrole &amp; gaz.
Dans l'optique d'une amélioration continue, je fais actuellement deux certifications en Gestion de Projets
selon les standards PMI et en Management Essentials à HARVARD BUSINESS SCHOOL." (:br)
"Aujourd'hui, mon challenge est plus que jamais de mettre mon expérience au service de mon pays, la Côte d'Ivoire.")
	  ((:a :href "https://www.linkedin.com/in/yves-sahi-ba395766/" :class "btn btn-primary") "Savoir plus")))
	((:div :class "card")
	 (:img :src "/imgs/david.jpg" :class "card-img-top" :alt "...")
	 ((:div :class "card-body")
	  ((:h5 :class "card-title") "David DAINGUI")
	  ((:p :class "card-text") "Après un brillant parcours au Lycée Classique d'Abidjan où j'obtiens le BAC C, j'intègre l'Institut National Polytechnique Félix HOUPHOUËT-BOIGNY (INPHB, Côte d'Ivoire), d'où j'obtiens le diplôme d'Ingénieur de conception en génie énergétique." (:br) "Je totalise 4 ans d'expérience dans les métiers de l'ingénierie électrique (production, dispatching, transport, etc.). Aujourd'hui, je suis consacré au Management la QSE, toujours dans le secteur électrique." (:br) "Je suis proactif et volontaire pour apporter mon expérience au développement énergétique de la Côte d'Ivoire et de toute l'Afrique.")
	  ((:a :href "https://www.linkedin.com/in/david-daingui-52765584/" :class "btn btn-primary") "Savoir plus"))))))))
