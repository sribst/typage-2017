# typage-2017

Dependence :
ocamlbuild oasis menhir ocamllex

Creation :
oasis setup
make

lancement :
pour lancer les tests dans le dossier example : ./test.sh
pour lancer le top : ./top.sh

pour lancer "à la main" :
pour le top : ./main.native -I
pour un fichier : ./main.native -S fichier_exemple bool
ou fichier exemple contient une expression par ligne
et bool = false | true pour l'affichage du systeme d'équation et du
mgu 