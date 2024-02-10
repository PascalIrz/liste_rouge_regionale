# Objectif

Repo qui reprend en partie https://github.com/PascalIrz/ipr_trends pour d'évaluer les tendances temporelles des poissons d'eau douce de Bretagne. Ce travail vient en appui de la révision de la liste rouge régionale.

# Approche

Nous produisons une sorte de tableau de bord combinant divers indicateurs de la dynamique des populations. Ces populations sont étudiées à travers leurs densités, leur taux d'occurrence et leur distributions en taille. Les indicateurs sont calculés pour soit séparément pour les juvéniles et les adultes, soit en les ciombinant.

Une première analyse statistique simple consiste à estimer les tendances temporelles des indicateurs par un test de Mann-Kendall (significativité d'une tendance monotone) suivi d'une régression de Sen-Theil (pente). Dans un second temps les indicateurs serviront à caler des modèles Bayésiens de dynamique de population.

Les caractéristiques des sites seront prises en compte pour analyser les résultats.

# Constitution du jeu de données

## Sélection des données

Il faut d'abord sélectionner, dans la base Aspe de l'OFB ([Irz et al. 2022](https://www.kmae-journal.org/articles/kmae/full_html/2022/01/kmae220057/kmae220057.html)) des sites en Bretagne qui ont été suivis pendant suffisamment longtemps et selon des protocoles adaptés pour estimer les densités. Sur ces sites, nous sélectionnons des séries de pêches avec au moins 10 années de données sans que les opérations successives soient distantes de plus de deux ans.

En complément de la base Aspe, un tableau indiquant pour chaque espèce la taille maximale des 0+ et la taille à maturité sexuelle est constitué.

## Calcul des indicateurs par opération de pêche

Certains des indicateurs sont d'abord calculés par pêche (ex : densité, longueur médiane, pourcentage de juvéniles) pour chaque espèce.

## Indicateurs régionaux

Pour les indicateurs calculés au point, on agrège chaque année leur valeur à l'échelle régionale.

Le taux d'occurrence est directement calculé annuellement comme le pourcentage des sites prospectés où l'espèce a été trouvée.

## Les données manquantes

Il apparaît que certaines données sont manquantes.

- Si certaines longueurs individuelles manquent, elles sont soit estimées soit remplacées par la taille médiane des individus sur le site.
- De nombreux poids manquent. On les estime donc par une relation taille-poids.
- Si des caractéristiques des sites manquent, elles sont remplacées par leur valeur moyenne

# Estimation des tendances temporelles

A compléter
