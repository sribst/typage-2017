# typage-2017
===================================

Projet du cours de 2017 de typage du M2
Langages et Programmation à Paris 7 - Diderot. 

But : inférence des types pour des types récursifs

Utilisation :
===================================

Dependence :
ocamlbuild oasis menhir ocamllex

Creation :
$ oasis setup
$ make

lancement :
pour lancer les tests dans le dossier example :
$ ./test.sh
pour lancer le top :
$ ./top.sh

pour lancer "à la main" :
pour le top :
$ ./main.native -I
sur un fichier :
$ ./main.native -S fichier_exemple BOOL
ou fichier exemple contient une expression par ligne
et BOOL = false | true pour l'affichage du systeme d'équation et du mgu 