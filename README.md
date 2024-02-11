
<!-- README.md is generated from README.Rmd. Please edit that file -->

# liste_rouge_regionale

<!-- badges: start -->
<!-- badges: end -->

# Objectif

Evaluer les tendances temporelles des poissons d’eau douce de Bretagne.
Ce travail vient en appui de la révision de la liste rouge régionale.

# Approche

Nous produisons une sorte de tableau de bord combinant divers
indicateurs de la dynamique des populations. Ces populations sont
étudiées à travers leurs densités, leur taux d’occurrence et leur
distributions en taille. Les indicateurs sont calculés pour soit
séparément pour les juvéniles et les adultes, soit en les combinant.

Une première analyse statistique simple consiste à estimer les tendances
temporelles des indicateurs par un test de Mann-Kendall (significativité
d’une tendance monotone) suivi d’une régression de Sen-Theil (pente).
Dans un second temps les indicateurs serviront à caler des modèles
Bayésiens de dynamique de population.

Les caractéristiques des sites seront prises en compte pour analyser les
résultats.

# Constitution du jeu de données

## Sélection des sites et des opérations de pêche

On appelle ici site le *point_prelevement* de la base Aspe, caractérisé
par son identifiant *pop_id*.

Dans la base Aspe de l’OFB ([Irz et
al. 2022](https://www.kmae-journal.org/articles/kmae/full_html/2022/01/kmae220057/kmae220057.html)),
nous sélectionnons des sites :

- Localisés en Bretagne.
- Qui ont été suivis pendant suffisamment longtemps et selon des
  protocoles adaptés pour estimer les densités.

Sur ces sites, nous sélectionnons des séries de pêches avec au moins 10
années de données sans que les opérations successives soient distantes
de plus de deux ans.

Si plusieurs pêches ont été réalisées la même année sur un site, on ne
retient que la plus tardive (pêche d’automne).

## Distinction des classes d’âge

En complément de la base Aspe, un tableau indiquant pour chaque espèce
la taille maximale des 0+ et la taille à maturité sexuelle est
constitué. On utilise donc la longueur du poisson comme *proxy* de son
âge.

## Calcul des indicateurs par opération de pêche

Certains des indicateurs sont d’abord calculés par pêche (ex : densité,
longueur médiane, pourcentage de juvéniles) pour chaque classe d’âge de
chaque espèce.

Chacun est stocké dans un tableau contenant les variables :

- *ope_id*
- *esp_code_alternatif* (code espèce à trois lettres)
- *classe_age* (classe d’âge codée “0+”, “adultes” ou bien “toutes”)
- *indicateur* (exemples : “longueur_mediane”, “densité surfacique”,
  etc.)
- *valeur* (valeur prise par l’indicateur pour cette opération)

![](assets/lm_ope.PNG)

Ces tableaux sont ensuite regroupés (empilés dans un tableau unique)
pour simplifier les analyses ultérieures. Ce tableau est complété pour
associer chaque opération à son site et à son année. Il contient donc,
outre les variables mentionnées précédemment, *pop_id* et *annee*.

![](assets/indicateurs_ope.PNG)

## Indicateurs régionaux

Pour les indicateurs calculés au point, on agrège chaque année leur
valeur à l’échelle régionale.

Le taux d’occurrence de chaque espèce est directement calculé
annuellement, à l’échelle régionale, comme le pourcentage des sites
prospectés où l’espèce a été trouvée.

Ces deux tableaux sont assemblés(empilés).

![](assets/indicateurs_annuels.PNG)

## Les données manquantes

Il apparaît que certaines données sont manquantes.

- Si certaines longueurs individuelles manquent, elles sont soit
  estimées, soit remplacées par la taille médiane des individus de la
  même espèce sur le site.
- De nombreux poids manquent. On les estime donc tous par une relation
  taille-poids.
- Si des caractéristiques des sites manquent, elles sont remplacées par
  leur valeur moyenne.

# Estimation des tendances temporelles

A compléter
