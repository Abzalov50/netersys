(defpackage :netersys
  (:use :cl :cl-serve :cl-serve.utils :cl-who)
  (:export :start-netersys
	   :stop-netersys))

(in-package :netersys)

(setf (html-mode) :html5)

(setf cl-serve:*domain* "netersys.com")
(setf cl-serve:*domain-ip* "172.105.81.235")
(set-subdomains nil)

(defvar *domain* cl-serve:*domain*)
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
(defun start-netersys (&key (cert *cert*) (privkey *privkey*))
  (start *domain* :cert cert :privkey privkey))

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
  (with-html-output-to-string (s)
    (:html
     (:head
      (:title "Page not found"))
     (:body
      (:p "The page you are requesting does not exist yet!")
      (:hr)
      (:i "cl-serve 1.0")))))

(defmacro generic-page (title &body body)
  `(with-html-output-to-string (s)
     ((:html :lang "fr")
      (:head
       (:script :type "text/javascript" :src "https://www.googletagmanager.com/gtag/js?id=UA-160220482-1" :async t)
       (:script :type "text/javascript":src "/js/gtag.js")
       (:title ,title " | netersys")
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
       (:link :rel "stylesheet"
	      :href "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.52.0/codemirror.min.css")
       (:link :rel "stylesheet"
	      :href "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.52.0/theme/blackboard.min.css")
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
	   (:li :class "nav-item" ((:a :class "nav-link" :href "/events") "Evénements"))
	   (:li :class "nav-item" ((:a :class "nav-link" :href "/forum") "Forum"))
	   (:li :class "nav-item" ((:a :class "nav-link" :href "/about")
				   ((:span :class "to-upper") "à") " propos"))
	   (:li :class "nav-item" ((:a :class "nav-link" :href "/contact") "Contact")))
	  ((:form :class "form-inline my-2 my-lg-0")
	   (:input :class "form-control mr-sm-2" :type "search" :placeholder "Recherche")
	   ((:button :class "btn btn-outline-success my-2 my-sm-0" :type "submit") "OK"))))
       ,@body
       (:footer
	((:div :class "main-content footer-main")
	 ((:div :id "footer-menu"))
	 ((:div :id "copyright")
	  "Copyright &copy; 2019-2020 " "netersys" ". Tous droits réservés.")))
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
       (:script :type "text/javascript" :src "https://polyfill.io/v3/polyfill.min.js?features=es6")
       (:script :type "text/javascript" :id "MathJax-script" :src "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js" :async t)
       (:script :type "text/javascript"
		:src "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.52.0/codemirror.min.js")
       (:script :type "text/javascript"
		:src "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.52.0/mode/python/python.min.js")
       (:script :type "text/javascript" :src "/js/main.js")
       ;;(:script :type "text/javascript" :src "https://code.jquery.com/jquery-3.4.1.min.js")
       ;;(:script :type "text/javascript" :src "https://code.jquery.com/ui/1.12.1/jquery-ui.min.js")
       ;;(:script :src "https://maps.googleapis.com/maps/api/js?key AIzaSyBhOdIF3Y9382fqJYt5I_sswSrEw5eihAA")
       ))))

;;; --------------------- ACCUEIL
(defhandler ("")
  (generic-page "Accueil"
    (:main
     ((:div :class "carousel slide carousel-fade"
	    :data-ride "carousel" :id "home-carousel")
      ;; Carousel Indicators
      ((:ol :class "carousel-indicators")
       (:li :data-target "#home-carousel" :data-slide-to "0"
	    :class "active")
       (:li :data-target "#home-carousel" :data-slide-to "1"))
      ;; Carousel slides
      ((:div :class "carousel-inner" :role "listbox")
       #|
       ((:div :class "carousel-item active")
	((:div :class "view")
	 (:img :class "d-block w-100" :height "500px"
	       :src "/imgs/flyer-bootcampRO2020.png"
	       :alt "Prospectus Boot Camp RO 2020")
	 (:div :class "mask rgba-black-light")))
|#
       ((:div :class "carousel-item active")
	((:div :class "view")
	 (:img :class "d-block w-100" :height "500px"
	       :src "/imgs/home-banner.jpg"
	       :alt "African nature")
	 (:div :class "mask rgba-black-strong"))
	((:div :class "carousel-caption about-content blog-header-content")
	 ((:h1 :class "text-white") "Build a system on top of nature... netersys !")
	 ((:a :href "/produits" :class "primary-btn") "Voir plus"))))
      ;; Carousel Controls
      ((:a :class "carousel-control-prev" :href "#home-carousel"
	   :role "button" :data-slide "prev")
       (:span :class "carousel-button-prev-icon" :aria-hidden "true")
       ((:span :class "sr-only") "Previous"))
      ((:a :class "carousel-control-next" :href "#home-carousel"
	   :role "button" :data-slide "next")
       (:span :class "carousel-button-next-icon" :aria-hidden "true")
       ((:span :class "sr-only") "Next")))
     ((:div :class "main-content")
      (:section
       (:h1 "Quoi de neuf ?")
       ((:p) "Toutes les infos sur le " (:strong "Boot Camp Recherche Opérationnelle 2020") ", " ((:a :href "/events") "ici."))
       ((:p) "Pour plus d'infos sur netersys, " ((:a :href "/about#netersys") "cliquez ici."))
       ((:p) "Prenez connaissance avec nos membres, " ((:a :href "/about#members") "ici.")))
      (:section
       (:h1 "Nos stats"))))))

;;; --------------------- A PROPOS
(defhandler ("about")
  (generic-page "A propos de nous"
    (:main :class "main-content main-without-banner"
     ((:section :id "netersys")
      ((:h1) "Sur " ((:span :class "netersys") "netersys"))
      ((:div :class "about-netersys")
      ((:p) ((:span :class "netersys-intext") "netersys") " est un outil d'aide à la décision, basé sur les données. C'est une application de l'optimisation et de l'intelligence artificielle à un ensemble de domaines d'intérêts (gestion des réseaux électriques, gestion de la production, gestion de la demande et de l'approvisionnement, gestion de la chaîne logistique, gestion des process, etc.)."
       (:br)
       "C'est un système construit au-dessus (on top) d'outils d'analyse mathématique existants, permettant de résoudre, dans un même framework, des problèmes théoriquement analogues, traités habituellement séparément. Il est par ce fait même, un outil générique.")
      ((:p) "Son architecture simplifiée se décline en 2 parties communiquantes: une base de données métier et un moteur de calcul-analyse-décision."
       (:br)
((:span :class "netersys-intext") "netersys") " est un outil purement logiciel, destiné à vivre dans le Cloud (un Cloudware, pour utiliser le vocabulaire geek), et essentiellement open source.")))
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

;;; --------------------- EVENEMENTS
(defhandler ("events")
  (generic-page "Evénements"
    ((:main :class "container main-without-banner")
     ((:div :class "row")
      ((:section)
       ((:h2) "Boot Camp Recherche Opérationnelle - Yakro 2020")
       ((:h5) "Mars 2020, INP-HB, Yamoussoukro, Côte d'Ivoire")
       ((:div)
	(:img :class "img-event"
	      :src "/imgs/flyer-bootcampRO2020.png"
	      :alt "Prospectus Boot Camp RO 2020")
	((:p) ((:span :class "see-more") ((:a :href "bootcampRO2020") "Cliquez ici")) " pour otenir le programme détaillé, le kit de préparation du Boot Camp, ...")))))))

;;; --------------------- BOOT CAMP RO 2020
(defhandler ("bootcampRO2020")
  (generic-page "Boot Camp RO 2020"
    ((:main :class "main-content main-without-banner")
     ((:div :class "myrow")
      ((:div :class "side")
       ((:h2) "Programme")
       ((:h3) "Théorie")
       ((:ol :class "ol-prog-up")
	((:li) "Généralités sur la Recherche Opérationnelle"
	 ((:ol :class "ol-prog-down")
	  (:li "Définition")
	  (:li "Objectifs")
	  (:li "Domaines d'application")))
	((:li) "Résolution des problèmes de RO"
	 ((:ol :class "ol-prog-down")
	  (:li "Développement d'un modèle")
	  (:li "Types de modèle")
	  (:li "Méthodes de résolution")))
	((:li) "La Programmation Linéaire (en Nombres Entiers)"
	 ((:ol :class "ol-prog-down")
	  (:li "Formulation d'un PL")
	  (:li "Méthode du simplexe")
	  (:li "Formulation d'un PLNE")
	  (:li "Branch and Bound"))))
       ((:h3) "Applications")
       ((:ol :class "ol-prog-up")
	(:li "Planification de la production dans une compagnie de cosmétique")
	(:li "Planification de la production dans un microgrid")
	(:li "Planification d'un réseau d'eau")
	(:li "Planification du transport")
	(:li "Optimisation de la production dans l'industrie de consommation")))
      ((:div :class "progcontent")
       ((:h1) "Prérequis du Boot Camp")
       ((:h3 :id "theory") "Théorie : Algèbre linéaire")
       ((:div)
	((:p)
	 ((:span :style "color:red;") "En cours de préparation..."))
	((:p) "Il ne s'agit pas ici de faire un cours complet d'Algèbre linéaire, mais plutôt d'en exposer quelques éléments qui sont nécessaires pour le Boot Camp. Ce sont entre autres, la notion d'espace vectoriel, de sous-espace vectoriel, d'espace engendré, de base, de dimension, d'espace euclidien, d'hyperplan et de matrice."
	 (:br)
	 "Commençons d'abord par donner une définition de l'algèbre linéaire.")
	((:p) "L'algèbre linéaire, c'est l'étude des " ((:em :class "keywrd") "transformations linéaires") " sur des " ((:em :class "keywrd") "espaces vectoriels") " de " ((:em :class "keywrd") "dimensions finies") ".")
	((:div :class "theorem")
	 ((:p) "Un " ((:em :class "keywrd") "espace vectoriel") " est un ensemble non vide sur lequel sont définies des opérations d'addition et de produit scalaire, vérifiant la propriété suivante :")
	((:p) "Soient \\(V\\) un espace vectoriel, \\(u, w \\in V\\) et \\(\\alpha\\in\\mathbb{R}\\). Alors, on a : 
$$u+w\\in V \\mbox{ et } \\alpha u\\in V$$"))
	((:p) "On dit que l'espace vectoriel est " ((:em :class "keywrd") "fermé") " sur l'addition et sur la mutiplication."
	 (:br)
	 "L'espace vectoriel doit en plus vérifier les propriétés suivantes (qui sont des axiomes) :")
	((:div :class "theorem")
	((:p) "Soient \\(V\\) un espace vectoriel, \\(u, v, w \\in V\\) et \\(\\alpha, \\gamma\\in\\mathbb{R}\\).")
	((:p) (:strong "Commutativité de l'addition:") " \\(u+w = w+u\\)" (:br)
	 (:strong "Associativité de l'addition:") " \\(u+(v+w) = (u+v)+w\\)" (:br)
	 (:strong (:span :class "to-upper" "é") "lément neutre de l'addition:") " Il existe un élément \\(\\mathbf{0}\\in V\\) tel que \\(u+\\mathbf{0} = u\\)" (:br)
	 (:strong (:span :class "to-upper" "é") "lément inverse de l'addition:") " Il existe un élément \\(w\\in V\\) tel que \\(u+w = \\mathbf{0}\\)" (:br)
	 (:strong (:span :class "to-upper" "é") "lément neutre de la multiplication (le scalaire 1):") " \\(1 u = u\\)" (:br)
	 (:strong "Distributivité:") " \\(\\gamma(u+w) = \\gamma u + \\gamma w \\mbox{ et } u(\\gamma + \\alpha) = \\gamma u + \\alpha u\\)" (:br)))
	((:p) "Il ressort donc de ces axiomes qu'un espace vectoriel doit contenir au minimum l'élément \\(\\mathbf{0}\\). Il suffit qu'un ensemble quelconque vérifie les propriétés susmentionnées pour être qualifié d'espace vectoriel. Un exemple trivial d'espace vectoriel est donc le singleton \\(\\{0\\}\\). En effet, cet ensemble est non vide et contient l'élément \\(0\\). Il est nécessairement fermé sur l'addition (\\(0 + 0 = 0\\in \\{0\\}\\)) et sur la multiplication (\\(0 \\times 0 = 0\\in \\{0\\}\\)). Vous pouvez vérifier que tous les axiomes sont trivialement vérifiés. Un autre exemple d'espace vectoriel est l'ensemble des nombres réel, le fameux \\(\\mathbb{R}\\). En effet, il contient un élément \\(0\\). La somme de 2 nombres réels est un nombre réel et le produit d'un scalaire (qui est aussi un nombre réel) par un nombre réel est un nombre réel. \\(\\mathbb{R}\\) est donc fermé sur l'addition et sur la multiplication. On sait tous que l'addition est commutative, càd si \\(a, b\\in\\mathbb{R} \\mbox{ alors } a+b = b+a\\). L'associativité est également vérifiée. le nombre réel \\(0\\) est l'élément neutre de l'addition, car \\(\\forall a\\in\\mathbb{R}, a + 0 = a\\). Chaque nombre réel \\(a\\) a un inverse \\(b\\) dans \\(\\mathbb{R}\\), tel que \\(a+b = 0\\). On sait que \\(b = -a\\). On sait que le nombre réel 1 est l'élément neutre de la multiplication, car \\(\\forall a\\in\\mathbb{R}, a \\times 1 = a\\). On sait aussi que la distributivité est vérifiée sur \\(\\mathbb{R}\\). Par conséquent, \\(\\mathbb{R}\\) est un espace vectoriel.")
	((:p) "Les éléments d'un espace vectoriel sont appelés des " (:em :class "keywrd" "vecteurs") " ou des " (:em :class "keywrd" "points") ".")
	((:p) "Un espace vectoriel sur le corps des réels \\(\\mathbb{R}\\), càd dont les éléments ou les éléments des éléments sont des réels, est appelé " (:em :class "keywrd" "espace vectoriel réel") ".")
	((:p) "Un sous-espace \\(U\\), d'un espace vectoriel \\(V\\), est qualifié de " (:em :class "keywrd" "sous-espace vectoriel") " si \\(U\\) est également un espace vectoriel.")
	((:div :class "theorem")
	 ((:p) "Soit un corps \\(E\\). Soit \\(E^n\\) l'ensemble des listes ordonnées de taille entière \\(n\\). Un élément de \\(E^n\\) a la forme \\((x_1, x_2, ..., x_n)\\), où \\(x_1, x_2, ..., \\mbox{ et } x_n\\) sont les composants de la liste, et appartiennent à \\(E\\). Soient \\(u = (u_1, u_2, ..., u_n)\\) et \\(v = (v_1, v_2, ..., v_n)\\) deux vecteurs de \\(E^n\\) et un scalaire \\(\\alpha\\in E\\). Associons à \\(E^n\\), les opérations d'addition et de multiplication définies par :
$$u+v = (u_1+v_1, u_2+v_2, ..., u_n+v_n) \\mbox{ et } \\alpha u = (\\alpha u_1, \\alpha u_2, ..., \\alpha u_n)$$
Et on voit bien que \\(u+v\\) et \\(\\alpha u\\) sont des listes ordonnées de taille \\(n\\), donc appartiennent également à \\(E^n\\).")
	 ((:p) "On définit aussi le vecteur \\(\\mathbf{0}\\) par une liste ordonnée de taille \\(n\\) composée uniquement de \\(0\\in E\\)."))
	 ((:p) "Nous affirmons que l'ensemble \\(E^n\\) est un espace vectoriel. Montrons pourquoi. Nous avons déjà défini un élément \\(\\mathbf{0}\\), donc l'ensemble n'est pas vide. L'addition, définie plus haut, est commutative, car en effet :
$$v+u = (v_1+u_1, v_2+u_2, ..., v_n+u_n)$$
Et comme l'addition \\(v_i + u_i\\) est commutative, alors :
$$v+u = (u_1+v_1, u_2+v_2, ..., u_n+v_n) = u+v$$
L'addition sur \\(E^n\\) est également associative ; cela se démontre trivialement. L'élément \\(\\mathbf{0}\\) est l'élément neutre. Tout élément \\(u\\) de \\(E^n\\) a un inverse \\(-u\\), défini par \\(-u = (-u_1, -u_2, ..., -u_n)\\). On voit que les \\(-u_i\\) sont des éléments du corps \\(E\\) et que \\(u-u = (u_1-u_1, u_2-u_2, ..., u_n-u_n)=\\mathbf{0}\\). On vérifie facilement que le scalaire 1 est un élément neutre de la multiplication. Je vous laisse vérifier l'axiome de distributivité.")
	((:p) "On vient de montrer que \\(E^n\\) est un espace vectoriel. Posons maintenant \\(E = \\mathbb{R}\\) ; ainsi \\(\\mathbb{R}^n\\) est un espace vectoriel. C'est en effet ce qu'on appelle l'" (:em :class "keywrd" "espace euclidien") ". Par exemple, \\(\\mathbb{R}^2 \\mbox{ et } \\mathbb{R}^3\\) sont les traditionnels plan et espace cartésiens.")
       ((:div :class "theorem")
	((:p) "On appelle " (:em :class "keywrd" "combinaison linéaire") " des vecteurs \\(u_1, u_2, ..., u_n\\), le vecteur \\(v = \\lambda_1 u_1 + \\lambda_2 u_2 + \\cdots + \\lambda_n u_n\\), où \\(\\lambda_1, \\lambda_2, ..., \\lambda_n\\) sont des scalaires."))
	((:div :class "theorem")
	 ((:p) "Un ensemble de vecteurs \\(\\{u_1, u_2, ..., u_n\\}\\) est dit " (:em :class "keywrd" "linéairement indépendant") " si \\(v = \\lambda_1 u_1 + \\lambda_2 u_2 + \\cdots + \\lambda_n u_n = \\mathbf{0}\\) implique que \\(\\lambda_i = 0, \\forall i=1, ..., n\\)"))
	((:p) "Ainsi, si on peut trouver des \\(\\lambda_i, \\forall i=1, ..., n\\) non tous nuls, tels que vecteur \\(\\lambda_1 u_1 + \\lambda_2 u_2 + \\cdots + \\lambda_n u_n=\\mathbf{0}\\), on dit que l'ensemble de vecteurs \\(\\{u_1, u_2, ..., u_n\\}\\) est " (:em :class "keywrd" "linéairement dépendant") ".")
	((:p) "Soit à déterminer la dépendance des vecteurs \\(v_1=(1,3,5), v_2=(2,5,9) \\mbox{ et } v_3=(-3,9,3)\\). On commence par former la combinaison linéaire de ces 3 vecteurs et l'égaler au vecteur \\(\\mathbf{0}\\). Soit \\(\\lambda_1, \\lambda_2 \\mbox{ et } \\lambda_3\\in\\mathbb{R}\\) tels que :
$$\\lambda_1 v_1 + \\lambda_2 v_2 + \\lambda_3 v_3 = \\mathbf{0}$$
Ou
$$\\lambda_1 (1,3,5) + \\lambda_2 (2,5,9) + \\lambda_3 (-3,9,3) = \\mathbf{0}$$
Pour plus de clarté, on dispose les vecteurs verticalement :
$$\\lambda_1 \\begin{pmatrix}1\\\\3\\\\5\\end{pmatrix} + \\lambda_2 \\begin{pmatrix}2\\\\5\\\\9\\end{pmatrix} + \\lambda_3 \\begin{pmatrix}-3\\\\9\\\\3\\end{pmatrix} = \\begin{pmatrix}0\\\\0\\\\0\\end{pmatrix}$$
Ce qui donne le système suivant :
\\begin{cases}
\\lambda_1 + 2 \\lambda_2 -3 \\lambda_3 &amp; = 0\\\\
3 \\lambda_1 + 5 \\lambda_2 +9 \\lambda_3 &amp; = 0\\\\
5 \\lambda_1 + 9 \\lambda_2 +3 \\lambda_3 &amp; = 0
\\end{cases}
L'application de l'élimination de Gauss donne les les systèmes équivalents suivants :
\\begin{array}{rcl}
\\begin{cases}
\\lambda_1 + 2 \\lambda_2 -3 \\lambda_3 &amp; = 0\\\\
-\\lambda_2 +18 \\lambda_3 &amp; = 0\\\\
-\\lambda_2 +18 \\lambda_3 &amp; = 0
\\end{cases}
& \\sim &
\\begin{cases}
\\lambda_1 + 2 \\lambda_2 -3 \\lambda_3 &amp; = 0\\\\
\\phantom{\\lambda_1 +}-\\lambda_2 +18 \\lambda_3 &amp; = 0
\\end{cases}
\\end{array}
Ce système ayant une infinité de solutions, dont le cas trivial où \\(\\lambda_1=\\lambda_2=\\lambda_3=0\\), on en déduit que les vecteurs \\((1,3,5), (2,5,9) \\mbox{ et } (-3,9,3)\\) sont dépendants.")
	((:p) "Soit maintenant à déterminer la dépendance des vecteurs \\(v_1=(1,0,0), v_2=(0,1,0) \\mbox{ et } v_3=(0,0,1)\\). Soit \\(\\lambda_1, \\lambda_2 \\mbox{ et } \\lambda_3\\in\\mathbb{R}\\) tels que :
$$\\lambda_1 v_1 + \\lambda_2 v_2 + \\lambda_3 v_3 = \\mathbf{0}$$
Ou
$$\\lambda_1 (1,0,0) + \\lambda_2 (0,1,0) + \\lambda_3 (0,0,1) = \\mathbf{0}$$
Pour plus de clarté, on dispose les vecteurs verticalement :
$$\\lambda_1 \\begin{pmatrix}1\\\\3\\\\5\\end{pmatrix} + \\lambda_2 \\begin{pmatrix}1\\\\0\\\\0\\end{pmatrix} + \\lambda_3 \\begin{pmatrix}0\\\\1\\\\0\\end{pmatrix} = \\begin{pmatrix}0\\\\0\\\\1\\end{pmatrix}$$
Ce qui donne le système suivant :
\\begin{cases}
\\lambda_1\\phantom{+ \\lambda_2 + \\lambda_3} &amp; = 0\\\\
\\phantom{\\lambda_1 +}\\lambda_2\\phantom{+\\lambda_3} &amp; = 0\\\\
\\phantom{\\lambda_1 + \\lambda_2 +}\\lambda_3 &amp; = 0
\\end{cases}
Ce système a une solution unique, qui est la solution triviale \\(\\lambda_1=\\lambda_2=\\lambda_3=0\\). On en déduit que les vecteurs \\((1,0,0), (0,1,0) \\mbox{ et } (0,0,1)\\) sont indépendants.")
	((:p) "On peut montrer de même que les vecteurs \\(v_1 = 1, v_2 = \\sin^2 x \\mbox{ et } v_3 = 1-2\\cos 2x\\) sont linéairement dépendants. Notez que ces vecteurs sont des fonctions. D'après les propriétés de trigonométrie, on sait que \\(\\cos 2x = \\cos^2 x - \\sin^2 x\\) or \\(\\cos^2 x + \\sin^2 x = 1\\), ce qui donne \\(\\cos^2 x = 1 - \\sin^2 x\\). Donc \\(\\cos 2x = 1 - \\sin^2 x - \\sin^2 x = 1 - 2\\sin^2 x\\). Ainsi, \\(v_3 = 1-2(1 - 2\\sin^2 x) = -1+4\\sin^2 x\\). On voit bien que \\(v_3 = -v_1 + 4 v_2\\). On en déduit que l'ensemble \\(\\{1,\\sin^2 x,1-2\\cos 2x\\}\\) est linéairement indépendant.")
	((:p) "On a dit qu'un espace vectoriel réel \\(V\\) est fermé sur l'addition et sur la multiplication, càd que si \\(u, w\\in V\\), alors \\(u+w\\in V\\) et \\(\\lambda u\\in V\\) (avec \\(\\lambda\\in \\mathbb{R}\\)). Cela revient à dire si \\(\\alpha u + \\lambda w \\in V\\) (avec \\(\\alpha, \\lambda\\in \\mathbb{R}\\)). En d'autres termes, toutes les combinaisons linéaires de 2 vecteurs d'un espace vectoriel sont des vecteurs du même espace. Ce concept de combinaison linéaire de 2 vecteurs s'étend, par induction (récurrence), à la combinaison linéaire d'un nombre arbitraire de vecteurs. Donc si, dans un espace vectoriel, on choisit un nombre arbitraire de vecteurs, leur combinaison linéaire donne un vecteur de cet espace.")
	((:p) "D'après ce qui a été dit dans le précédent paragraphe, on peut prendre des vecteurs, et à partir d'eux, construire un espace vectoriel. Il suffit de construire l'ensemble des combinaisons linéaires de ces vecteurs. On dit que cet ensemble initial " (:em :class "keywrd" "engendre") " l'espace vectoriel, et on l'appelle " (:em :class "keywrd" "une base") " de cet espace. Un espace vectoriel peut avoir plusieurs bases. Mais, on démontre que toutes les bases d'un espace vectoriel ont la même taille (le même nombre d'éléments), et contiennent des vecteurs linéairement indépendants. En d'autres termes, si l'ensemble \\(\\{u_1, u_2, ..., u_n\\}\\) est une base d'un espace vectoriel, l'ensemble \\(\\{u_1, u_2, ..., u_n, u_{n+1}\\}\\) ne peut pas en être une. Car le vecteur \\(u_{n+1}\\) est nécessairement généré par la combinaison des autres vecteurs (Se référer à tout manuel d'algèbre linéaire pour la démonstration). La taille d'une base est fixe : cette taille est appelée " (:em :class "keywrd" "la dimension") " de l'espace vectoriel.")
	((:p) "Nous avons vu que l'ensemble \\(\\{(1,0,0), (0,1,0), (0,0,1)\\}\\) est linéairement indépendant. En effet, ces 3 vecteurs forment la base naturelle de l'espace cartésien de dimension. On déduit par induction (on le démontre de la même manière que précédemment) que l'ensemble \\(\\{e_1, e_2, ..., e_n\\}\\subset\\mathbb{R}^n\\), tel que \\(e_i\\) est un vecteur dont la \\(i\\)-ème composante est égale à 1, et les autres composantes sont nulles, est une base naturelle de l'espace euclidien \\(\\mathbb{R}^n\\).")
	)
;  "When \\(a \\ne 0\\), there are two solutions to \\(ax^2 + bx + c = 0\\) and they are
;  \\[x = {-b \\pm \\sqrt{b^2-4ac} \\over 2a}.\\]")
       (:br)
       ((:h3 :id "applications") "Tutoriels vidéos de préparation des outils informatiques")
       ((:h4) "Tuto 1: Mise en place d'un environnement de programmation Python et d'optimisation")
       ((:div :class "embedresize")
	(:div
	 (:iframe
	  :width "560" :height "315" :frameborder "0"
	  :src "https://youtube.com/embed/Pngrf1T1Ueg"
	  :allow "accelerometer;encrypted-media;gyroscope;picture-in-picture"
	  :allowfullscreen t)))
       (:br)(:br)
       ((:h4) "Tuto 2: " ((:span :class "to-upper") "é") "criture et exécution de programmes Python")
       ((:div :class "embedresize")
	(:div
	 (:iframe
	 :width "468" :height "263" :frameborder "0"
	 :src "https://youtube.com/embed/_7mLHNnaMlI"
	 :allow "accelerometer;encrypted-media;gyroscope;picture-in-picture"
	 :allowfullscreen t)))
       (:br)(:br)
       ((:h4) "Tuto 3: Prise en main de l'environnement d'optimisation mathématique SCIP")
       ((:div)
	((:p) ((:span :style "color:red;") "En cours de préparation...")))
       (:br)
       ((:h3 :id "applications") "Applications : Bases de la programmation Python")
       ((:div)
	((:p) "Il s'agit ici de faire une introduction à la programmation Python. Nous supposons que le lecteur n'a même pas de notions en programmation ; aussi, nous commencerons par définir, de façon très générale, ce qu'est la programmation, avant de nous attaquer à Python.")
	((:p) "Dans le jargon informatique, " (:em :class "keywrd" "Programmer") ", c'est faire faire à un matériel (généralement l'ordinateur) ce qu'on souhaite. Cette requête est un ensemble d'instructions écrites dans un langage (appelé " (:em :class "keywrd" "langage de programmation") ") compris par ce matériel. Cet ensemble d'instructions est appelé " (:em :class "keywrd" "programme") ". Si ce programme ne contient aucune erreur (ou " (:em :class "keywrd" "bogue") "), l'ordinateur accomplit ce qu'il lui a été demandé.")
	((:p) "Il existe plus d'une centaine de langages de programmation, chacune ayant ses avantages et ses inconvénients. Chaque langage porte un nom, dû à ses créateurs. Les langages les plus anciens sont ceux des familles ALGOL, FORTRAN et LISP. Mais, le langage Python est aujourd'hui le langage le plus populaire (non pas le plus puissant). Ce langage s'est forgé une réputation dans le calcul scientifique. C'est le langage par excellence de la Data Science. C'est donc un langage approprié pour ce Boot Camp.")
	((:h4) "Les variables")
	((:p) "En général, on n'a pas recours à l'ordinateur pour des tâches qui peuvent être trivialement accomplies manuellement. On en a recours pour des problèmes difficiles ou pour automatiser des tâches répétitives. Un problème comporte des informations ou " (:em :class "keywrd" "données") " (" (:em :class "keywrd" "data") ", en anglais). Dans un programme informatique, pour faciliter le référencement d'une donnée (sa valeur), on lui attribue un nom ou identifiant. Le couple " (:strong "(identifiant, valeur)") " est appelé une " (:em :class "keywrd" "variable") ". Lorsque la valeur d'une variable est constante, c'est-à-dire qu'elle ne change pas, cette variable est simplement qualifiée de " (:em :class "keywrd" "constante") ".")
	((:p) "La valeur d'une donnée peut être aussi simple qu'un nombre (entier ou décimal) ou un caractère alphabétique. Mais, elle peut être un peu plus complexe, telle qu'une chaîne de caractères ou une liste de données. En mathématiques, on dirait que la valeur d'une donnée appartient à un certain intervalle. En programmation informatique, on parle de " (:em :class "keywrd" "type") " de données. Les types, que nous avons mentionnés précédemment, sont qualifiés de simples, car ils sont assez naturels. Il y a en outre une autre classe de types, dits " (:em :class "keywrd" "types abstraits") ". Ce sont des types construits à partir des types simples. Ces types ne sont pas aussi intuitifs que les types simples." (:br)
	 "Les types simples sont généralement primitifs à tous les langages de programmation, c'est-à-dire conçus par les créateurs du langage. Mais, les types abstraits ne le sont pas. Ils sont conçus par les utilisateurs du langage.")
	((:p) "Dans tous les tous les langages de programmation, une des opérations (ou instructions) les plus élémentaires est la création d'une variable, c'est-à-dire l'association d'une valeur et d'un identifiant. Cette opération est appelée " (:em :class "keywrd" "assignation") " ou " (:em :class "keywrd" "affectation") " ou " (:em :class "keywrd" "liaison") ". En Python, on crée une variable en utilisant la syntaxe suivante :")
	((:pre)
	 ((:code :class "python_code short_code")
	  "prenom = 'Arnold'
age = 29
taille = 1.75"))
	((:p) "Dans cet exemple, nous déclarons 3 variables, d'identifiants " ((:span :class "varident") "prenom") ", " ((:span :class "varident") "age") " et " ((:span :class "varident") "taille") " , et de valeurs 'Arnold', 29 et 1.75, respectivement. Chaque langage définit les règles de nommage d'une variable. Celles de Python sont accessibles " ((:a :href "https://www.python.org/dev/peps/pep-0008/#naming-conventions") "ici.")
	 (:br)
	 "La valeur 'Arnold' est de type chaîne de caractères (ou " (:em :class "keywrd" "string") "). 29 est de type entier (ou " (:em :class "keywrd" "integer") ") et 1.75 est de type décimal ou flottant (ou " (:em :class "keywrd" "float") ").")
	((:p) "Dans un programme, une variable a une " (:em :class "keywrd" "portée") ", càd une zone dans laquelle elle est accessible, et une " (:em :class "keywrd" "durée de vie") ". Concernant la portée, on distingue 2 types de variables : les variables " (:em :class "keywrd" "globales") " et les variables " (:em :class "keywrd" "locales") ". Les premières sont accessibles presque partout dans le programme, et les dernières ne le sont qu'à l'intérieur de certains environnements, dont on parlera plus tard. Les variables, que nous avons définies plus haut, sont des variables globales. Le concept de durée de vie est beaucoup plus complexe, et rarement compris, même par des programmeurs chevronnés. Nous n'en parlons donc pas dans cette introduction.")
	((:p) "Dès qu'une variable est créée, elle peut être référencée ailleurs dans le programme, en fonction de sa portée. Elle peut donc servir à créer d'autres variables, comme dans l'exemple ci-après :")
	((:pre)
	 ((:code :class "python_code short_code")
	  "age_prochain = age + 1"))
	((:p) "Dans cet exemple, on crée une variable d'identifiant " ((:span :class "varident") "age_prochain") " , et dont la valeur est égale à la valeur de la variable de nom " ((:span :class "varident") "age") "plus 1. L'opérateur + est l'opérateur mathématique d'addition. La variable " ((:span :class "varident") "age_prochain") "aura donc 30 comme valeur.")
	((:p) "Comme on peut le voir sur les exemples précédents, en Python, on sépare 2 instructions par un retour à la ligne. Il est possible d'aligner toutes les instructions sur une seule ligne, en les séparant par un point-virgule. Mais, cette approche n'est pas recommandée car elle réduit la lisibilité du programme. On a également vu qu'une variable peut servir à créer d'autres variables, en utilisant des opérateurs définis par le langage. Ces opérateurs peuvent porter sur des nombres, des chaînes de caractères, ou tout autre type de données.")
	((:h4) "Les boucles")
	((:p) "Nous avons dit plus haut que l'un des objectifs de la programmation est d'aider à l'automatisation des tâches répétitives. On s'imagine donc que chaque langage doit fournir des moyens pour faire ça. Un de ces moyens est ce qu'on appelle une " (:em :class "keywrd" "boucle") " ou une " (:em :class "keywrd" "structure itérative") ". Le mot \"itérative\" dérive du verbe \"itérer\", qui signifie \"repéter\".")
	((:p) "En Python, il y a 2 structures itérative : la boucle " (:strong "for") " et la boucle " (:strong "while") ". Soit à écrire un programme permettant d'afficher 100 fois la phrase \"Je ne bavarderai plus en classe.\". On pourrait l'écrire comme suit :")
	((:pre)
	 ((:code :class "python_code short_code")
	  "for i in range(100):
    print(\"Je ne bavarderai plus en classe.\")"))
	((:p) "La ligne 1 commence par le mot-clé (du langage) " (:strong "for") ", indiquant que ce qui suit, est une structure de boucle de type " (:strong "for") ". La ligne se termine par un deux-point, indiquant que ce qui suit est le corps de la structure. Entre " (:strong "for") " et le deux-point, on définit l'itérateur " ((:span :class "varident") "i") "et l'ensemble qu'il parcourt, càd dont il prend successivement la valeur de chaque élément. En effet, la ligne 1 signifie littéralement \"Pour chaque valeur de la variable " ((:span :class "varident") "i") " dans l'ensemble " ((:span :class "varident") "range(100)") ", faire ... Et " ((:span :class "varident") "range(100)") " est l'ensemble des nombres entiers naturels de 0 à 99. 'instruction. En Python, tous les ensembles qui peuvent être parcourus dans une boucle " (:strong "for") " sont appelés " (:em :class "keywrd" "itérables") ". Quelques types primitifs sont des itérables (voir cette " ((:a :href "#iterables") "section") "), mais les utilisateurs du langage peuvent en créer." 
	 (:br)
	 "Toutes les structures Python ont la même syntaxe. Vous vous en convaincrez que nous en verront d'autres. Le corps de la structure peut comporter plusieurs instructions, mais dans cet exemple, il y en a qu'une seule (ligne 2). L'instruction " ((:span :class "varident") "print(...)") " affiche la donnée qui se trouve entre les parenthèses. Dans cet exemple, c'est la chaîne de caractères \"Je ne bavarderai plus en classe.\". Chaque fois que l'itérateur prend une nouvelle valeur dans son ensemble, le corps de la structure est parcouru, càd que toutes les instructions qui y sont contenues, sont exécutées. C'est la notion de boucle. Remarquez que la variable " ((:span :class "varident") "i") " n'est pas référencée dans le corps. Cette situation est très rare. Remarquez également que le corps de la structure n'est pas verticalement aligné avec le debut de la ligne 1. On dit que le corps est " (:em :class "keywrd" "indenté") ". En Python, l'indentation n'est ni arbitraire ni optionnelle, elle est obligatoire et vaut 4 caractères espace. La version " (:strong "while") " du précédent programme peut être exprimée comme suit :"
	 ((:pre)
	  ((:code :class "python_code med_code")
	  "i = 0
while i < 100:
    print(\"Je ne bavarderai plus en classe.\")
    i = i + 1"))
	 ((:p) "Cette version est plus longue que la précédente , en nombre de lignes que nous avons écrits. " ((:span :class "to-upper") "à") " la ligne 1, on crée une variable " ((:span :class "varident") "i") " et on l'initialise à 0. La seconde ligne débute la structure de boucle " (:strong "while") ". Cette ligne veut littéralement dire \"Tant que " ((:span :class "varident") "i") " est strictement inférieur à 100, faire...\". Dans le corps de la boucle, après l'affichage du texte, on assigne une nouvelle valeur à " ((:span :class "varident") "i") " (ligne 4), qui est son ancienne valeur plus 1. Remarquez que, de cette façon, le corps de la boucle est parcouru 100 fois exactement, car sa valeur initiale est 0 et sa valeur finale est 99, prenant toutes les valeurs entières entre ces 2 bornes.")

	 ((:h4 :id "iterables") "Quelques types itérables: string, liste, tuple et dictionnaire")
	 ((:p) "Nous avons déjà rencontré le type string (chaîne de caractères). En Python, c'est une suite de caractères alphanumériques ou spéciaux, délimitée par des guillemets doubles ou simples. " ((:span :class "varident") "\"Arnold N'GORAN\"") " et " ((:span :class "varident") "'Boot Camp Recherche Opérationnelle'") " sont 2 chaînes de caractères valides. On note que l'espace est un caractère valide, et que les caractères non alphabétiques (dits caractères spéciaux) sont également valides. Parmi les caractères spéciaux, on distingue ceux dits " (:em :class "keywrd" "séquences d'échappement") ". Par exemple, " ((:span :class "varident") "\n") "est le caractère \"retour à la ligne\" et " ((:span :class "varident") "\t") " est le caractère \"tabulation\". Le caractère \"\\\" (appelé " (:em :class "keywrd" "backlash") ") est un " (:em :class "keywrd" "caractère d'échappement") ". Il permet d'insérer les guillemets doubles et le backlash, lui-même, dans une chaîne de caractères. Il suffit de précéder ces caractères par le backslash. Le type string est un itérable car il est équivalent à un ensemble ordonné, composé des caractères qui le composent.")
	 ((:p) "Les types " ((:span :class "varident") "liste") " et " ((:span :class "varident") "tuple") " sont des listes ordonnés d'éléments dont chacun peut être de n'importe quel type Python. Les éléments d'une donnée de type liste, sont délimités par des crochets, et ceux de type tuple, sont délimités par des parenthèses. Les éléments sont séparés par des virgules. Voici quelques exemples :")
	 ((:pre)
	  ((:code :class "python_code med_code")
	  "couleurs = [\"bleu\", \"rouge\", \"vert\", \"jaune\"]
booleens = (0, 1)
multidata = [12.3, \"Arnold\", [1, 2, 3]]"))
	 ((:p) "La ligne 3 montre bien qu'une liste peut comporter une autre liste. La différence enre le type " ((:span :class "varident") "liste") " et le type " ((:span :class "varident") "tuple") " est au niveau de ce qu'on appelle la " (:em :class "keywrd" "mutabilité") ", càd la qualité de ce qui est modifiable. En effet, le contenu d'une liste est modifiable, alors que celui d'un tuple ne l'est pas. On dit que les listes sont mutables et les tuples sont non mutables.")
	 ((:p) "Les types " ((:span :class "varident") "string") ", " ((:span :class "varident") "liste") " et " ((:span :class "varident") "tuple") ", étant des ensembles ordonnés d'éléments, chacun de ces éléments a donc une position. Le premier élément est à la position 0, le second élément est à la position 1, etc. La syntaxe pour accéder à un élément, connaissant sa position, est : " ((:span :class "varident") "nom_de_la_varable[position]") ". Par exemple, " ((:span :class "varident") "couleurs[1]") " donne l'élément \"rouge\", et " ((:span :class "varident") "multidata[2]") " donne l'élément [1, 2, 3]."
	  (:br)
	  "Python fixe la position du dernier élément à " (:strong "-1") ", ce qui permet d'y accéder facilement.")
	 ((:p) "Un dictionnaire est un type de donnée de la forme suivante : " ((:span :class "varident") "{cle1: valeur1, cle2: valeur2, ..., cleN: valeurN}") ". C'est donc suite de clés-valeurs, séparés par une virgule, le tout délimité par des accolades. Les clés sont uniquement de type non mutable. En d'autres termes, une clé ne peut pas être de type " ((:span :class "varident") "liste") ". Un dictionnaire peut être vu comme une liste dont les clés sont les positions des éléments.")
	 ((:pre)
	  ((:code :class "python_code short_code")
	  "infos_arnold = {'nom': \"N'GORAN\", 'prenom': 'Arnold', 'age': 29, 'email': 'arnoldngoran@gmail.com'}
print(infos_arnold['age'])   # affiche 29"))
	 ((:p) "L'exemple est assez compréhensible. Mais, il y a une nouveauté, c'est l'apparition du caractère \"#\" et d'un texte qui le suit. C'et ce qu'on appelle un " ((:em :class "keywrd") "commentaire") " en Python. Un commentaire est une chaîne de caractères sont exécutés ; c'est tout simplement ignoré. Il sert juste d'indications pour le programmeur et les utilisateurs du programme."
	  (:br)
	  "Les dictionnaires sont également des itérables. Lorsqu'on itère sur un dictionnaire, via une boucle " (:strong "for") ", on itère sur l'ensemble (non ordonné) de ces clés. Par exemple :")
	 ((:pre)
	  ((:code :class "python_code short_code")
	  "for key in infos_arnold:
    print(key, \"=>\", infos_arnold[key])"))
	 ((:p) "Ce qui fournit le résultat suivant :")
	 ((:pre)
	  ((:code :class "python_result med_code")
	  "nom => N'GORAN
prenom => Arnold
age => 29
email => arnoldngoran@gmail.com"))
	 ((:p) ((:span :class "to-upper") "à") " la ligne 1, on itère sur le dictionnaire " ((:span :class "varident") "infos_arnold") ". L'utilisation de " ((:span :class "varident") "print") " à la ligne 2 est différente de celle des autres exemples. Cette fois, on lui passe, entre parenthèses, plusieurs données, séparées par des virgules. Ces données peuvent des références de variables. Python affiche ces données dans l'ordre, en séparant 2 données consécutives par un espace. Et chaque fois que " ((:span :class "varident") "print") " est appelé, càd à chaque itération, l'affichage commence à une nouvelle ligne.")
	 ((:h4) "Le type booléen")
	 ((:p) "Le type booléen sert à donner la valeur de vérité d'une proposition ou expression. On considère qu'une proposition est vraie ou fausse. En Python, ces valeurs sont " ((:span :class "varident") "True") " et " ((:span :class "varident") "False") ", respectivement. Ce sont les deux seules données (constantes) de type booléen en Python. De ce type, ce sont des " ((:em :class "keywrd") "mots réservés") " du langage, càd qu'ils ne peuvent pas être utilisés comme identifiants de variable. Il convient de dire ici que les noms des variables (ou fonctions...), respectent la casse. En d'autres termes, " ((:span :class "varident") "True") ", " ((:span :class "varident") "true") ", et " ((:span :class "varident") "TRUE") " sont des identifiants différents. Seul le premier est le mot réservé de Python. Les autres peuvent être utilisés comme identifiants.")
	 ((:p) "Comme on l'a dit au précédent paragraphe, une proposition est une expression du langage qui est soit vraie soit fausse. Il y a un certain nombre d'opérateurs Python qui permettent de construire des propositions, et ainsi de faire des calculs propositionnels, comme en logique mathématique. Nous allons voir quelques exemples.")
	 ((:pre)
	  ((:code :class "python_code")
	  "A = True
B = True
C = not(B)    # C = False
D = A or B    # D = True
E = A or C    # E = True
F = A and C   # F = False
G = 12 == 7   # G = False
H = 12 != 7   # H = True
I = True and (12+3 == 15)   # I = True"))
	 ((:p) "L'opérateur " ((:span :class "varident") "not") " prend un seul paramètre (une proposition), et retourne valeur de vérité opposée à celle de cette proposition. L'opérateur " ((:span :class "varident") "or") " est le \"OU\" logique, et " ((:span :class "varident") "and") " est le \"ET\". Le double égal \"==\" teste l'égalité de deux expressions, et l'opérateur \"!=\" teste si deux expressions sont différentes.")
	 ((:h4) "La structure conditionnelle \"if\"")
	 ((:p) "Tous les langages de programmation offrent des moyens d'exprimer l'exécution conditionnelle de certains instructions. En Python, c'et la structure " (:strong ((:span :class "varident") "if")) " qui le permet. La syntaxe est la suivante :")
	 ((:pre)
	  ((:code :class "python_code")
	  "if condition_1:
    instruction_1
elif condition_2:
    instruction_2
.................
.................
elif condition_N:
    instruction_N
else:
    default_instruction"))
	 ((:p) "Littéralement, cette structure se lit : \"Si " ((:span :class "varident") "condition_1") " est vérifiée, exécuter " ((:span :class "varident") "instruction_1") ", sinon si " ((:span :class "varident") "condition_2") " est vérifiée, exécuter " ((:span :class "varident") "instruction_2") ", ..., " ((:span :class "varident") "condition_N") " est vérifiée, exécuter " ((:span :class "varident") "instruction_N") ", sinon exécuter " ((:span :class "varident") "default_instruction") ". Prenons un exemple pour illustrer l'utilisation de cette structure.")
	 ((:pre)
	  ((:code :class "python_code")
	  "nombres = [22, 15, -1, 0, -43, 0.0, 9]
for i in nombres:
    if i < 0:
        print(i, 'est un nombre négatif')
    elif i > 0:
        print(i, 'est un nombre positif')
    else:
        print(i, 'est un nombre nul')")))
	((:p) ((:span :class "to-upper") "à") " la ligne 1, on déclare une liste identifiée par " ((:span :class "varident") "nombres") ". Ensuite, à la ligne 2, on itère sur cette liste, avec " ((:span :class "varident") "i") " comme variable d'itération. De la ligne 3 à la ligne 8, nous avons une structure conditionnelle, à 3 branches. Pour chaque valeur de " ((:span :class "varident") "i") " dans la liste " ((:span :class "varident") "nombres") ", si " ((:span :class "varident") "i < 0") ", on affiche que ce nombre est négatif (première branche), sinon si " ((:span :class "varident") "i > 0") ", on affiche que ce nombre est négatif (deuxième branche), sinon (càd si " ((:span :class "varident") "i = 0") ") on affiche que ce nombre est nul."
	 (:br)
	 "Le précédent donne comme résultat :")
	((:pre)
	 ((:code :class "python_result medplus_code")
	  "22 est un nombre positif
15 est un nombre positif
-1 est un nombre négatif
0 est un nombre nul
-43 est un nombre négatif
0.0 est un nombre nul
9 est un nombre positif"))
	((:h4) "Les fonctions")
	((:p) "Nous avons appris que la création d'une variable permet de donner un nom à une donnée, et ainsi de pouvoir référencer cette donnnée, ailleurs dans le programme. Les langages de programmation offrent une fonctionnalité équivalente pour associer un nom à tout une portion de programme, afin de pouvoir la réutiliser ailleurs : on l'appelle " ((:em :class "keywrd") "fonction") " ou " ((:em :class "keywrd") "procédure") ". Cette notion de fonction est analogue à celle rencontrée en maths : une fonction Python peut dépendre d'un certains nombre de variables, appelés spécialement des " ((:em :class "keywrd") "paramètres") ". Mais, il y a des différences notables. Une fonction Python peut ne pas retourner de valeurs, pour certaines valeurs de ses paramètres. " ((:span :class "to-upper") "à") " chaque exécution d'une fonction, avec les mêmes valeurs des paramètrs, toute chose égale par ailleurs, les valeurs des autres variables du programme ne sont pas nécessairement les mêmes. Dans ce second cas, on dit que les fonctions permettent des " ((:em :class "keywrd") "effets de bord") ". Les fonctions mathématiques ne permettent d'effets de bord.")
	((:p) "Voici la syntaxe de création d'une fonction Python :")
	((:pre)
	  ((:code :class "python_code short_code")
	  "def nom_de_la_fonction(param1, param2, ..., paramN):
    corps_de_la_fonction
    return valeur_a_retourner   # instruction optionnelle"))
	((:p) "Les critères de nommage d'une fonction sont les mêmes que ceux de nommage d'une variable. Une fonction a zéro ou plusieurs paramètres. Le corps de la fonction contient une suite d'instructions Python, du même genre que nous avons vu jusqu'ici. La dernière instruction d'une fonction est une instruction normale ou une instruction " ((:span :class "varident") "return") ". Si cette instruction apparaît, elle indique le nombre de valeurs que la fonction retourne, chaque fois qu'on l'exécute. Si cette instruction n'apparaît pas, on dit que la fonction ne retourne pas de valeurs."
	 ((:br) "Cette phase est appelée " ((:em :class "keywrd") "définition de la fonction") ". Elle permet d'indiquer à Python que nous souhaitons créer une fonction avec la définition donnée. Ensuite, nous pourrons utiliser cette fonction (selon sa portée). Cette phase est appelée " ((:em :class "keywrd") "appel de la fonction") ". La syntaxe d'un appel de fonction est : " ((:span :class "varident") "nom_de_la_fonction(arg1, arg2, ..., argN)") ", où " ((:span :class "varident") "arg1, arg2, ..., ") " et " ((:span :class "varident") "argN") " sont les valeurs des paramètres de la fonction, " ((:span :class "varident") "param1, param2, ..., ") " et " ((:span :class "varident") "paramN") ", respectivement."))
	((:p) "Prenons comme exemple la fonction valeur absolue, définie mathématiquement comme suit :
$$f(x) = |x| = 
\\begin{cases}
x &amp; \\mbox{si } x\\geq 0\\\\
-x &amp; \\mbox{si } x < 0
\\end{cases}$$")
	((:p) "Cette fonction peut être programmée comme suit :")
	((:pre)
	  ((:code :class "python_code medplus_code")
	  "def valeur_absolue(x):
    if x >= 0:
        f = x
    else:
        f = -x
    return f"))
	((:p) "La fonction étant définie, nous pouvons l'appel dans l'exemple suivant :")
	((:pre)
	  ((:code :class "python_code short_code")
	  "for i in nombres:
    print('La valeur absolue de', i, 'est :', valeur_absolue(i))"))
	((:p) "Ce qui donne le résultat suivant :")
	((:pre)
	 ((:code :class "python_result medplus_code")
	  "La valeur absolue de 22 est : 22
La valeur absolue de 15 est : 15
La valeur absolue de -1 est : 1
La valeur absolue de 0 est : 0
La valeur absolue de -43 est : 43
La valeur absolue de 0.0 est : 0.0
La valeur absolue de 9 est : 9"))
	((:p) "Pour terminer cette introduction à la programmatio Python, considérons un exemple qui permet d'appliquer toutes les notions apprises. Il s'agit d'écrire un programme qui résout une équation du second degré, et informe l'utilisateur sur les solutions de l'équation. Si l'équation n'admet pas de solution, il le dit simplement.")
	((:p) "Commençons d'abord par rappeler les solutions d'une équation du second degré, de la forme : \\(a x^2 + b x + c = 0\\), avec \\(a\\neq 0\\). Pour résoudre cette équation, on commence par calculer le discriminant \\(\\Delta = b^2 - 4 a c\\). Si \\(\\Delta > 0\\), l'équation admet deux solutions \\(x_1\\) et \\(x_2\\), définies par :
$$x_1 = \\frac{-b - \\sqrt{\\Delta}}{2 a} \\mbox{ et } x_2 = \\frac{-b + \\sqrt{\\Delta}}{2 a}$$

Si \\(\\Delta = 0\\), l'équation admet une solution double \\(x_0\\), définie par :
$$x_0 = \\frac{-b}{2 a}$$

Cependant, si \\(\\Delta < 0\\), l'équation n'admet pas de solutions.")
	((:p) "Le programme, que nous devons écrire, peut comporter une fonction qui prend comme paramètres, les coefficients \\(a, b\\) et \\(c\\), et retourne les solutions, si l'équation en admet ou un indicateur d'absence de solutions, sinon. Voici une implémentation de cette fonction :")
	((:pre)
	  ((:code :class "python_code long_code")
	  "from math import sqrt

def solution_eq_2nd_degre(a, b, c):
    # Calcul du discriminant
    D = b*b - 4 * a * c
    
    # Discussion du nombre de solutions
    # en fonction du signe du discriminant
    if D > 0:
        x_1 = (-b - sqrt(D)) / (2 * a)
        x_2 = (-b + sqrt(D)) / (2 * a)
        res = (x_1, x_2)    # La variable \"res\" stocke les solutions
    elif D == 0:
        x_0 = -b / (2 * a)
        res = (x_0,)   # Lorsque le tuple contient un seul élement,
                       # il doit être suivi par une virgule.
    else:   # Si D < 0
        res = None   # Indique qu'il n'y a pas de solutions

    return res"))
	((:p) "Ce programme contient quelques nouveautés. Cela commence à la première ligne. En effet, presque tous les langages permettent de stocker des programmes dans un fichier, afin qu'ils puissent être utilisés dans des programmes, contenus dans d'autres fichiers. Cela s'appelle la " ((:em :class "keywrd") "modularité") ". En Python, ces fichiers sont appelés des " ((:em :class "keywrd") "modules") ". Les concepteurs du langage fournissent des modules, qu'on appelle modules " ((:em :class "keywrd") "primitifs") ". Un de ces modules est le module " ((:span :class "varident") "math") ", qui contient un grand nombre de fonctions et constantes mathématiques, dont notamment la fonction racine carrée, nommée " ((:span :class "varident") "sqrt") ", utilisée dans cet exemple. L'instruction de la ligne 1 veut littéralement dire \"depuis le module " ((:span :class "varident") "math") ", je souhaite importer la fonction " ((:span :class "varident") "sqrt") ", afin de l'utiliser dans ce programme\". Si on voulait également appeler les fonctions sinus et arctangente du même module, on aurait déclaré : " ((:span :class "varident") "from math import sqrt, sin, atan") "."
	 (:br)
	 "La seconde remarque est à la ligne 5. En mathématique, on est autorisé à omettre l'opérateur de multiplication en algèbre, mais c'est preque jamais le cas en programmation. En Python, le symbole étoile \"*\" est utilisé comme opérateur de multiplication. Il doit donc explicitement être utilisé. Le terme \\(b^2\\) a été exprimé comme \\(b\times b\\, mais Python a un opérateur de puissance, le double étoile \"**\", donc on aurait pu écrire " ((:span :class "varident") "b**2") "."
	 (:br)
	 "La troisième remarque se situe aux lignes 10 et 11. On fait usage de parenthèses pour indiquer la priorité des opérations, comme on le fait en maths. On appelle la fonction racine carrée, qui prend un seul paramètre."
	 (:br)
	 "La quatrième remarque est au niveau de la ligne 13. On utilise le symbole \"==\" au lieu \"=\". En effet, lorsque c'est une condition, on utilise le double égal."
	 (:br) "La dernière remarque est à la ligne 18. On attribue à la variable " ((:span :class "varident") "res") ", la valeur " ((:span :class "varident") "None") ", qui est d'un type spécial de Python. Cette valeur signifie \"rien\". Si vous tentez de l'afficher à partir de la fonction " ((:span :class "varident") "print") ", rien ne s'affiche. On l'utilise ici pour indiquer qu'il n'y a pas de solutions.")
	((:p) "La fonction " ((:span :class "varident") "solution_eq_2nd_degre") " étant écrite, on peut écrire une fonction qui prend les mêmes paramètres, détermine les solutions et affiche les résultats. On peut l'implémenter comme suit :")
	((:pre)
	  ((:code :class "python_code medlong_code")
	  "def prog_eq_2nd_degre(a, b, c):
    # Test de confirmité des arguments
    if a == 0:
       print('ERREUR: Le premier argument doit être différent de 0')
       return

    # Détermination des solutions
    sol = solution_eq_2nd_degre(a, b, c)
    
    # Affichage des solutions
    if sol is None:
        print(\"L'équation n'admet pas de solutions\")
    elif len(sol) == 1:
        print(\"L'équation admet une solution double:\", sol[0])
    else:
        print(\"L'équation admet deux solutions:\", sol[0], \"et\", sol[1])"))
	((:p) "Cette seconde fonction comporte également quelques nouveautés.  D'abord, à la ligne 3, on teste si les arguments sont valides. Spécifiquement si " ((:span :class "varident") "a = 0") ", l'équation n'est pas de second degré, donc on affiche un message d'erreur, et on fait un " ((:span :class "varident") "return") ", pour sortir de la fonction. " ((:span :class "to-upper") "à") " la ligne 8, on appelle la fonction " ((:span :class "varident") "solution_eq_2nd_degre") " et on stocke les solutions dans la variable " ((:span :class "varident") "sol") ". Les lignes 11 à 16 sont des branches conditionnelles. La première branche teste s'il n'y a pas de solutions. On utilise l'opérateur " ((:span :class "varident") "is") " au lieu du double égal, pour tester l'identité. Cette condition est plus forte que l'égalité, mais on aurait pu utiliser le double égal. La seconde branche teste si la taille de la variable " ((:span :class "varident") "sol") " est égal à 1, càd que le tuple contient un seul élément. En effet, la fonction " ((:span :class "varident") "len") ", prenant un seul paramètre, permet de trouver la taille d'un itérable, càd son nombre d'éléments. On sait déjà que " ((:span :class "varident") "sol[0]") " et " ((:span :class "varident") "sol[1]") "donnent les premier et deuxième élément de la variable " ((:span :class "varident") "sol") "."
	 (:br) "Ces fonctions peuvent être testées par les instructions suivantes :")
	((:pre)
	  ((:code :class "python_code med_code")
	  "prog_eq_2nd_degre(1, 1, 1)
prog_eq_2nd_degre(0, -1, 2)
prog_eq_2nd_degre(1, 5, 4)
prog_eq_2nd_degre(2, -4, 2)"))
	((:p) "On a les résultats suivants:")
	((:pre)
	 ((:code :class "python_result med_code")
	  "L'équation n'admet pas de solutions.
ERREUR: Le premier argument doit être différent de 0
L'équation admet deux solutions: -4.0 et -1.0
L'équation admet une solution double: 1.0"))
	))))))

;;; --------------------- CONTACT
(defhandler ("contact")
  (generic-page "Contactez-nous"
    (:main
     ((:div :class "container contact-form")
      ((:form :method "post" :action "contact/send-message")
       (:h3 "Laissez-nous un message")
       ((:div :class "row")
	((:div :class "col-md-6")
	 ((:div :class "form-group")
	  (:input :type "text" :name "txtName"
		  :class "form-control" :placeholder "Nom *"
		  :value ""))
	 ((:div :class "form-group")
	  (:input :type "text" :name "txtEmail" :class "form-control"
		  :placeholder "Email *" :value ""))
	 ((:div :class "form-group")
	  (:input :type "text" :name "txtPhone" :class "form-control"
		  :placeholder "Téléphone *" :value ""))
	 ((:div :class "form-group")
	  (:input :type "submit" :name "btnSubmit"
		  :class "btnContact" :value "Envoyer")))
	((:div :class "col-md-6")
	 ((:div :class "form-group")
	  (:textarea :name "txtMsg" :class "form-control"
		     :placeholder "Votre Message *"
		     :style "width: 100%; height: 150px;")))))))))

(defhandler ("contact/send-message" :content-type "lambda")
  (destructuring-bind (name email phone message)
      (mapcar #'(lambda (c)
		  (get-assoc-value c cl-serve::params))
	      '("TXTNAME" "TXTEMAIL" "TXTPHONE" "TXTMSG"))
    (let ((metadata (list name email phone))
	  (from (concatenate 'string "visitor@" *domain*)))
      (get-email message from
		 '("netersys@gmail.com" "infos@netersys.com")
		 :metadata metadata)))
  )
