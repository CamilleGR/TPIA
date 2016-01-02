;;;;####################################################################
;;;;
;;;;                 SOURCE CONTENANT NOTRE BASE DE RÈGLES
;;;;
;;;;####################################################################


#|
SI ((!ChantierFaisable)
ET (LargeurTunnel*DureeChantier >= LongueurTunnel))
ALORS ((ChantierFaisable VRAI) (EquipeDeNuit Faux))
SI ((!ChantierFaisable)
ET (LargeurTunnel*DureeChantier*2 >= LongueurTunnel))
ALORS ((ChantierFaisable VRAI) (EquipeDeNuit Vrai))
SI (!ChantierFaisable)
ALORS (ChantierFaisable FAUX)
SI ((ChantierFaisable = VRAI) ) ALORS (NbNainMinier = LargeurTunnel)
SI ((NbNainMinier)) ALORS (NbNainGuerisseur NbNainMinier/3)
SI ((NbNainMinier)) ALORS (NbNainForgeron NbNainMinier/3)
SI ((NbNainMinier)) ALORS (NbNainTourneurManche NbNainMinier/3)


_______________

Variables :


// Paramètres :
NombreDeJours : Nombre de jours maximum du chantier.
LongueurTunnel : Longueur du tunnel (en mètres).
LargeurTunnel : Largueur du tunnel (en mètres).
HauteurTunnel : Hauteur du tunnel (en mètres).
TypeDeRoche : Type de roche du tunnel. CF tableau ci-dessous.
TypeDePioche : Type de pioche utilisé par les nains. CF tableau ci-dessous.

// Résultat final :
ChantierRéalisable : Booléen définissant si les paramètres sont réalisables.
PrésenceEquipeNuit : Booléen définissant si une équipe de nuit est nécessaire pour terminer le projet en temps et en heures.
NbNainMinier : Nombre de nains miniers.
NbNainGuerisseur : Nombre de nains guérisseurs.
NbNainForgeron : Nombre de nains forgerons.
NbNainTourneurManche : Nombre de nains tourneurs de manche.
NbNainSurveillant : Nombre de nains surveillants.
NbNainManager : Nombre de nains managers.
NbNainPorteurLanterne : Nombre de nains porteurs de lanterne.
NbNainRavitaillement : Nombre de nains servant la bière.
NbNainPlongueur : Nombre de nains plongeurs.
NbNainTotal : Résultat final du nombre de nains.

// Variables intermédiaires :
VitesseNain : Vitesse par nain par jour en fonction du type de roche et du type de pioche.

_________________________

Règles :

|#

;; Vitesse roche
(setq *BaseRegles*
          '(
						(
							(
								(VitesseNain . 4)
							)
							(
								(EGALITE (TypeDeRoche Micachiste))
							)
						RTR1)

						(
							(
								(VitesseNain . 3)
							)
							(
								(EGALITE (TypeDeRoche Granite))
							)
						RTR2)

						(
							(
								(VitesseNain . 3)
							)
							(
								(EGALITE (TypeDeRoche Gabbros))
							)
						RTR3)

						(
							(
								(VitesseNain . 3)
							)
							(
								(EGALITE (TypeDeRoche Prasinites))
							)
						RTR4)

						(
							(
								(VitesseNain . 3)
							)
							(
								(EGALITE (TypeDeRoche Serpentines))
							)
						RTR5)

						(
							(
								(VitesseNain . 3)
							)
							(
								(EGALITE (TypeDeRoche Cipolins))
							)
						RTR6)

						(
							(
								(VitesseNain . 2)
							)
							(
								(EGALITE (TypeDeRoche Amphiobolite))
							)
						RTR7)

						(
							(
								(VitesseNain . 2)
							)
							(
								(EGALITE (TypeDeRoche Leptyrites))
							)
						RTR8)

						(
							(
								(VitesseNain . 2)
							)
							(
								(EGALITE (TypeDeRoche Ophiolites))
							)
						RTR9)

						(
							(
								(VitesseNain . 2)
							)
							(
								(EGALITE (TypeDeRoche Orthophyres))
							)
						RTR10)

						(
							(
								(VitesseNain . 1)
							)
							(
								(EGALITE (TypeDeRoche Splites))
							)
						RTR11)

						(
							(
								(VitesseNain . 1)
							)
							(
								(EGALITE (TypeDeRoche Greis))
							)
						RTR12)

						(
							(
								(VitesseNain . (* VitesseNain 0.75))
							)
							(
								(DEFINI (VitesseNain))
								(EGALITE (TypeDePioche MauvaiseQualite))
							)
						RTP1)
						;; Vitesse pioche
						(
							(
								(VitesseNain . (* VitesseNain 1.5))
							)
							(
								(DEFINI (VitesseNain))
								(EGALITE (TypeDePioche Double))
							)
						RTP2)

						(
							(
								(VitesseNain . (* VitesseNain 2))
							)
							(
								(DEFINI (VitesseNain))
								(EGALITE (TypeDePioche Mithril))
							)
						RTP3)
					;; Chantier OK et equipe de nuit ?
						(
							(
								(ChantierRéalisable . T)
								(EquipeDeNuit . NIL)
							)

							(
								(COMPARAISON (>=
									(* LargeurTunnel HauteurTunnel NombreDeJours VitesseNain)
									LongueurTunnel
								))
							)
						RCR1)

						(
							(
								(ChantierRéalisable . T)
								(EquipeDeNuit . T)
							)

							(
								(COMPARAISON (>=
									(* LargeurTunnel HauteurTunnel NombreDeJours VitesseNain 2)
									LongueurTunnel (* LargeurTunnel HauteurTunnel NombreDeJours VitesseNain)
								))
							)
						RCR2)

						(
							(
								(ChantierRéalisable . NIL)
								(EquipeDeNuit . NIL)
							)

							(
								(COMPARAISON (<=
									(* LargeurTunnel HauteurTunnel NombreDeJours VitesseNain 2)
									LongueurTunnel
								))
							)
						RCR3)
						;; Calcul de nains
						(
							(
								(NbNainMinier .
								(*
										(truncate (/ LargeurTunnel 1.25))
										HauteurTunnel)
									)
								(NbNainGuerisseur .
								(ceiling
									(/ NbNainMinier 3))
								)
								(NbNainForgeron .
								(ceiling
									(/ NbNainMinier 3))
								)
								(NbNainTourneurManche .
								(ceiling
									(/ NbNainMinier 3))
								)
								(NbNainTotal .
								(+ NbNainMinier NbNainGuerisseur NbNainForgeron NbNainTourneurManche)
								)
							)

							(
								(EGALITE (ChantierRéalisable T))
							)
						RN1)

						(
							(
								(NbNainRavitaillement .
								(* NbNainMinier 4)
								)
								(NbNainPlongueur .
								(ceiling (/ NbNainRavitaillement 4))
								)
								(NbNainTotal .
								(+ NbNainTotal NbNainRavitaillement NbNainPlongueur)
								)
								(NainsCalculé . T)
							)

							(
								(DEFINI (NbNainMinier))
								(EGALITE (EquipeDeNuit NIL))
							)
						RN2)

						(
							(
								(NbNainPorteurLanterne .
								(* (ceiling (/ NbNainMinier  3)) 4)
								)
								
								(NbNainMinier .
								(* NbNainMinier 2)
								)
								
								(NbNainGuerisseur .
								(* NbNainGuerisseur 2)
								)
								
								(NbNainForgeron .
								(* NbNainForgeron 2)
								)
								
								(NbNainTourneurManche .
								(* NbNainTourneurManche 2)
								)
								
								(NbNainTotal
								(+ (* NbNainTotal 2) NbNainPorteurLanterne)
								)
								
								(NbNainSurveillant .
								(ceiling (/ NbNainTotal 4))
								)
								(NbNainManager . 
								(ceiling (/ NbNainSurveillant 3))
								)
								(NbNainRavitaillement .
								(* NbNainMinier 8)
								)
								(NbNainPlongueur .
								(ceiling (/ NbNainRavitaillement 4))
								)
								
								(NbNainTotal .
								(+ NbNainSurveillant NbNainManager NbNainRavitaillement NbNainPlongueur)
								)
								(NainsCalculé . T)
							)

							(
								(DEFINI (NbNainMinier))
								(EGALITE (EquipeDeNuit T))
							)
						RN3)

						(
							(
								;; TODO
							)

							(
								(EGALITE (NainsCalculé T))
							)
						RC)
					)

)

					;; Il manque : nains ravitaillements / nains surveillants / nains porteurs de lumière
					;; Le coût
					;; Règle finale qui calcule le nb de nains total
