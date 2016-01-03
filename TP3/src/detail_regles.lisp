(defparameter *detailRegle* '(
;;;; Regles Type de Roches
  (RTR1 . "Si la roche est de type Micachiste, alors le nain avancera avec une vitesse de 4.")
  (RTR2 . "Si la roche est de type Granite, alors le nain avancera avec une vitesse de 3.")
  (RTR3 . "Si la roche est de type Gabbros, alors le nain avancera avec une vitesse de 3.")
  (RTR4 . "Si la roche est de type Prasinites, alors le nain avancera avec une vitesse de 3.")
  (RTR5 . "Si la roche est de type Serpentines, alors le nain avancera avec une vitesse de 3.")
  (RTR6 . "Si la roche est de type Cipolins, alors le nain avancera avec une vitesse de 3.")
  (RTR7 . "Si la roche est de type Amphiobolite, alors le nain avancera avec une vitesse de 2.")
  (RTR8 . "Si la roche est de type Leptyrites, alors le nain avancera avec une vitesse de 2.")
  (RTR9 . "Si la roche est de type Ophiolites, alors le nain avancera avec une vitesse de 2.")
  (RTR10 . "Si la roche est de type Orthophyres, alors le nain avancera avec une vitesse de 2.")
  (RTR11 . "Si la roche est de type Splites, alors le nain avancera avec une vitesse de 1.")
  (RTR12 . "Si la roche est de type Greis, alors le nain avancera avec une vitesse de 1.")
  ;;;; Regles Type de Pioche
  (RTP1 . "Si la pioche est de mauvaise qualitée, alors la vitesse du nain est multipliée par 0.75")
  (RTP2 . "Si la pioche est une pioche double, alors la vitesse du nain est multipliée par 1.5")
  (RTP3 . "Si la pioche est en mithril, alors la vitesse du nain est multipliée par 2")
  (RTP4 . "Si la pioche est standard, alors la vitesse du nain est multipliée par 1")
  ;;;; Regles de chantier realisable
  (RCR1 . "Si LongueurTunnel <= LargeurTunnel*hauteurTunnel*nbJourMax*VitesseNain, le chantier est réalisable et nous n'avons pas besoin d'équipe de nuit")
  (RCR2 . "Si LargeurTunnel*hauteurTunnel*nbJourMax*VitesseNain <= LongueurTunnel <= LargeurTunnel*hauteurTunnel*nbJourMax*VitesseNain*2 , le chantier est réalisable et nous avons besoin d'une équipe de nuit")
  (RCR3 . "Si LongueurTunnel > LargeurTunnel*hauteurTunnel*nbJourMax*VitesseNain*2, alors le chantier n'est pas réalisable")
  ;;;; Regles des Nains
  (RN1 . "Si le chantier est réalisable, alors on crée le début de l'équipe de jour qui sera présente dans tous les cas (nain miniers, nains guérisseurs, nains tourneurs de manche et nains forgeron).")
  (RN2 . "Si il n'y a pas d'équipe de nuit et que l'équipe de jour est commencé, on complète l'équipe de jour avec les nains de ravitaillement et les nains plongueurs.")
  (RN3 . "Si une équipe de nuit est nécessaire et que l'équipe de jour est commencé, on crée l'équipe de nuit (Nains porteur de lanterne, nains surveillant, nains manager), on double l'effectif de l'équipe de jour pour avoir l'équivalence en équipe de nuit et on termine par les nains de ravitaillement et les nains plongueurs. ")

  )
)
