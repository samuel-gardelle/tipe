
# Compilation du Lambda-Calcul en Assembleur, d'un point de vue catégorique

Le projet est détaillé dans le fichier `rapport.pdf`

## Idée

L'idée de ce TIPE est d'utiliser l'algèbre de telle manière que l'on puisse
trouver le processus de compilation d'un langage en un autre en résolvant
une équation.

### Analogie avec la théorie des groupes

Supposons que l'on dispose d'un langage G. Que l'ensemble G soit en plus
doté d'une structure de groupe fini.
Soit x1, ..., xn une famille d'éléments de G. On souhaite calculer leur
produit.

Deux méthodes s'offrent à nous. Soit on connait la table de G et on peut
algorithmiquement calculer le produit x1 * ... * xn.

Soit on utilise le théorème de Cayley (tout groupe fini est isomorphe à un
sous-groupe du groupe des permutations).
Considérons H un sous-groupe du groupe des permutations et f un isomorphisme
de G dans H.
Pour calculer le produit x1 * ... * xn on peut "passer dans H" avec f, calculer
le produit dans H en utilisant la table de la loi de H, puis "revenir dans G"
en utilisant l'inverse de f.

On aura en quelque sorte compilé G en H (via f), puis interprété le sens du produit
x1 * ... * xn  dans H. Les axiomes de la théorie des groupes jouant ici le rôle
de la sémantique du langage G.

De plus f peut être trouvé en résolvant mathématiquement une équation entre
des groupes. On peut donc raisonner mathématiquement sur f.

Le seul problème ici est que la théorie des groupes n'est pas adaptée pour décrire
un langage de programmation.

### La théorie des catégories

La théorie des catégories est une théorie qui a la vertue de pouvoir décrire des objets
que ne sont pas étudiables avec d'autres théories de l'algèbre. Une structure algébrique
particulière, les catégories cartésiennes fermées, est connue pour sa capacité à décrire
le lambda-calcul simplement typé.

Le but de ce TIPE est de compiler le lambda-calcul simplement typé en assembleur.
Il s'agit donc de d'abord rechercher cette structure de catégorie cartésienne fermée
dans l'assembleur, puis d'en déduire un "morphisme" (foncteur) du lambda-calcul simplement typé
en assembleur qui préserve le sens des termes.

Dans l'analogie précédente avec les groupes, on pourra remarquer que plus G est doté d'une
structure algébrique avec beaucoup d'axiomes (par exemple si G était cyclique alors f serait
déterminé par l'image d'un monogène), moins il y a de morphismes possibles entre G et H, et
donc plus l'équation est facile à résoudre.
La structure de catégorie cartésienne fermée impose beaucoup d'axiomes et donc limite le choix.

Ce TIPE propose donc trois choses :
- identifier dans l'assembleur la structure de catégorie cartésienne fermée
- en déduire la compilation du lambda-calcul simplement typé en assembleur
- proposer une implémentation en Haskell
