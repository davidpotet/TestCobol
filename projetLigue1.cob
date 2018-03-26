           IDENTIFICATION DIVISION.
           PROGRAM-ID. main.

           ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
           FILE-CONTROL.

           SELECT fvisiteurs ASSIGN TO "fvisiteurs.dat"
           ORGANIZATION INDEXED
           ACCESS IS dynamic
           RECORD KEY fv_Visiteur
           ALTERNATE RECORD KEY fv_email WITH DUPLICATES
           FILE STATUS IS fvisiteurs_stat.

            SELECT factions ASSIGN TO "actions.dat"
            ORGANIZATION indexed
            ACCESS IS dynamic
            RECORD KEY IS fac_Action
            ALTERNATE RECORD KEY IS fac_idRencAc
            ALTERNATE RECORD KEY IS fac_idJouAc
            FILE STATUS IS factions_stat.

                SELECT farbitres ASSIGN TO "arbitres.dat"
            ORGANIZATION indexed
            ACCESS IS dynamic
            RECORD KEY IS fa_Arbitre
            ALTERNATE RECORD KEY IS fa_idRenc
            ALTERNATE RECORD KEY IS fa_dateRenc WITH DUPLICATES
            ALTERNATE RECORD KEY IS fa_heureRenc WITH DUPLICATES
            FILE STATUS IS farbitres_stat.

                SELECT fjoueurs ASSIGN TO "joueurs.dat"
            ORGANIZATION indexed
            ACCESS IS dynamic
            RECORD KEY IS fj_Joueur
            ALTERNATE RECORD KEY IS fj_idEq WITH DUPLICATES
            ALTERNATE RECORD KEY IS fj_accronymeEq WITH DUPLICATES
            FILE STATUS IS fjoueurs_stat.

                SELECT fequipes ASSIGN TO "equipes.dat"
            ORGANIZATION indexed
            ACCESS IS dynamic
            RECORD KEY IS fe_Equipe
            ALTERNATE RECORD KEY IS fe_idStade
            ALTERNATE RECORD KEY IS fe_lieuStade WITH DUPLICATES
            FILE STATUS IS fequipes_stat.

           SELECT fstades ASSIGN TO "stades.dat"
            ORGANIZATION indexed
            ACCESS IS dynamic
            RECORD KEY IS fs_Stade
            ALTERNATE RECORD KEY IS fs_idEquipe
            FILE STATUS IS fstades_stat.

                SELECT frencontres ASSIGN TO "rencontres.dat"
            ORGANIZATION indexed
            ACCESS IS dynamic
            RECORD KEY IS fr_Rencontre
           ALTERNATE RECORD KEY fr_dateRenc WITH DUPLICATES
           ALTERNATE RECORD KEY fr_statutRenc WITH DUPLICATES
            FILE STATUS IS frencontres_stat.

               SELECT fadministrateurs ASSIGN TO "administrateurs.dat"
           ORGANIZATION sequential
           ACCESS IS sequential
           FILE STATUS IS fadministrateurs_stat.


           DATA DIVISION.
           FILE SECTION.



           FD fadministrateurs.
            01 administrateurTampon.
              02 fa_mdp PIC X(15).


           FD fvisiteurs.
            01 fvisiteursTampon.
              02 fv_Visiteur.
                    03 fv_id PIC 9(3).
                    03 fv_nom PIC A(20).
              02 fv_prenom PIC A(20).
              02 fv_adresse PIC X(50).
              02 fv_email PIC X(30).
              02 fv_age PIC 9(3).

                FD factions.
            01 actionsTampon.
              02 fac_Action.
                03 fac_idAc PIC 9(3).
                03 fac_typeAc PIC A(20).
              02 fac_idRencAc PIC 9(3).
              02 fac_idJouAc PIC 9(3).
              02 fac_minuteAc PIC 9(3).


                    FD farbitres.
            01 arbitresTampon.
              02 fa_Arbitre.
                03 fa_idArb PIC 9(3).
                03 fa_nomArb PIC A(20).
              02 fa_dateRenc PIC X(10).
              02 fa_heureRenc PIC X(5).
              02 fa_idRenc PIC 9(3).
              02 fa_prenomArb PIC A(20).
              02 fa_ageArb PIC 9(2).
              02 fa_nationaliteArb PIC A(20).


                    FD fjoueurs.
            01 joueursTampon.
              02 fj_Joueur.
                03 fj_nomJou PIC A(20).
                03 fj_idJou PIC 9(3).
              02 fj_idEq PIC 9(3).
              02 fj_accronymeEq PIC A(3).
              02 fj_prenomJou PIC A(20).
              02 fj_ageJou PIC 9(2).
              02 fj_nationaliteJou PIC A(20).
              02 fj_numeroJou PIC 9(3).
              02 fj_posteJou PIC A(3).
              02 fj_butJou PIC 9(3).
              02 fj_passeJou PIC 9(3).
              02 fj_cartonRJou PIC 9(3).
              02 fj_cartonJJou PIC 9(3).


            FD fequipes.
            01 equipesTampon.
              02 fe_Equipe.
                03 fe_idEquipe PIC 9(6).
                03 fe_nomEq PIC A(20).
              02 fe_idStade PIC 9(6).
              02 fe_lieuStade PIC 9(6).
              02 fe_accronymeEq PIC A(3).
              02 fe_capitaineEq PIC 9(3).
              02 fe_entraineurEq PIC A(20).
              02 fe_pointsEq PIC 9(3).
              02 fe_butMarquesEq PIC 9(3).
              02 fe_butPrisEq PIC 9(3).
              02 fe_presidentEq PIC A(20).


           FD frencontres.
            01 rencontresTampon.
            02 fr_Rencontre.
                    03 fr_idRenc PIC 9(5).
                    03 fr_EquipeDom PIC 9(2).
                    03 fr_EquipeExt PIC 9(2).
            02 fr_heureRenc PIC X(5).
            02 fr_statutRenc PIC X(5).
            02 fr_scoreDomRenc PIC 9(2).
            02 fr_scoreExtRenc PIC 9(2).
            02 fr_dateRenc.
                    03 fr_anneeRenc PIC 9(4).
                    03 fr_moisRenc PIC 9(2).
                    03 fr_joursRenc PIC 9(2).
            02 fr_idArb1 PIC 9(2).
            02 fr_idArb2 PIC 9(2).
            02 fr_idArb3 PIC 9(2).
            02 fr_idArb4 PIC 9(2).


           FD fstades.
            01 stadesTampon.
              02 fs_Stade.
                      03 fs_idStade PIC 9(6).
                      03 fs_nomStade PIC 9(6).
              02 fs_idEquipe PIC 9(6).
              02 fs_lieuStade PIC A(20).
              02 fs_capaciteStade PIC 9(6).


           WORKING-STORAGE SECTION.

           77 factions_stat PIC 9(2).
           77 farbitres_stat PIC 9(2).
           77 fjoueurs_stat PIC 9(2).
           77 fequipes_stat PIC 9(2).
           77 frencontres_stat PIC 9(2).
           77 fstades_stat PIC 9(2).
           77 fvisiteurs_stat PIC 9(2).
           77 fadministrateurs_stat PIC 9(2).





           77 Wrep PIC 9.
           77 Wfin PIC 9.
           77 Wchoix PIC X(6).

           77 idJoueur PIC A(30).
           77 idArbitre PIC A(30).

           77 Wmdp PIC X(15).
           77 Wtestmdp PIC X(15).
           77 Wmotdepasse PIC X(15).
           77 Wnouveaumdp PIC X(15).


           77 WchoixVisiteur PIC 9(2).
           77 WchoixUtilisateur PIC 9(2).
           77 WchoixAdmin PIC 9(2).

           77 WJour_courant PIC 9(2).
           77 WMois_courant PIC 9(2).
           77 WAnnee_courant PIC 9(2).

           77 WJour_FinSaison PIC 9(2).
           77 WMois_FinSaison PIC 9(2).
           77 WAnnee_FinSaison PIC 9(2).


           77 WId_visiteur PIC 9(2).

           PROCEDURE DIVISION.
       MOVE 12 TO WJour_courant
       MOVE 8 TO WMois_courant
       MOVE 16 To WAnnee_courant

       MOVE 20 TO WJour_FinSaison
       MOVE 5 TO WMois_FinSaison
       MOVE 17 To WAnnee_FinSaison

        OPEN I-O factions
        IF factions_stat =35 THEN
          OPEN OUTPUT factions
        END-IF
        CLOSE factions

        OPEN I-O farbitres
        IF farbitres_stat =35 THEN
          OPEN OUTPUT farbitres
        END-IF
        CLOSE farbitres

        OPEN I-O fjoueurs
        IF fjoueurs_stat =35 THEN
          OPEN OUTPUT fjoueurs
        END-IF
        CLOSE fjoueurs

        OPEN I-O fequipes
        IF fequipes_stat =35 THEN
          OPEN OUTPUT fequipes
        END-IF
        CLOSE fequipes

        OPEN I-O frencontres
        IF frencontres_stat =35 THEN
            OPEN OUTPUT frencontres
        END-IF
        CLOSE frencontres

        OPEN I-O fstades
        IF fstades_stat =35 THEN
          OPEN OUTPUT fstades
        END-IF
        CLOSE fstades



           MOVE "administrateurs" TO Wmdp
           PERFORM MENU_PRINCIPAL
           STOP RUN.
            COPY "actions".
            COPY "arbitres".
            COPY "joueurs".
            COPY "equipes".
            COPY "rencontres".
            COPY "stades".


           MENU_PRINCIPAL.
           PERFORM WITH TEST AFTER UNTIL WchoixUtilisateur < 1
           DISPLAY
           '*****************************************************'
           DISPLAY
           '* Etes vous visiteur ou administrateur ?            *'
           DISPLAY
           '*                                                   *'
           DISPLAY
           '* 0 - Quitter                                       *'
           DISPLAY
           '*                                                   *'
           DISPLAY
           '* 1 - Je suis visiteur                              *'
           DISPLAY
           '*                                                   *'
           DISPLAY
           '* 2 - Je suis administrateur                        *'
           DISPLAY
           '*                                                   *'
           DISPLAY
           '*****************************************************'
           ACCEPT WchoixUtilisateur
               EVALUATE WchoixUtilisateur
                 WHEN 1 PERFORM AFFICHAGE_VISITEUR
                 WHEN 2 PERFORM AFFICHAGE_ADMINISTRATEUR
               END-EVALUATE
           END-PERFORM.


           AFFICHAGE_VISITEUR.
           PERFORM WITH TEST AFTER UNTIL WchoixVisiteur < 1
           DISPLAY
           '*****************************************************'
           DISPLAY
           '* Que voulez vous faire ?                           *'
           DISPLAY
           '*                                                   *'
           DISPLAY
           '* 0 - Quitter                                       *'
           DISPLAY
           '*                                                   *'
           DISPLAY
           '* 1 - Connexion                                     *'
           DISPLAY
           '*                                                   *'
           DISPLAY
           '* 2 - Créer un compte visiteur                      *'
           DISPLAY
           '*                                                   *'
           DISPLAY
           '* 3 - Deconnexion                                   *'
           DISPLAY
           '*                                                   *'
           DISPLAY
           '*****************************************************'
           ACCEPT WchoixVisiteur
           EVALUATE WchoixVisiteur
           WHEN 1
                  PERFORM CONNEXION_VISITEUR
           WHEN 2
                  PERFORM AJOUT_VISITEUR
           WHEN 3
                  PERFORM DECONNEXION_VISITEUR
           END-EVALUATE
           END-PERFORM.


           DECONNEXION_VISITEUR.
           MOVE 0 TO WId_visiteur
           PERFORM MENU_PRINCIPAL.

           AFFICHAGE_ADMINISTRATEUR.
           PERFORM WITH TEST AFTER UNTIL WchoixAdmin < 1
           DISPLAY
           '*****************************************************'
           DISPLAY
           '* Quel fichier voulez-vous consulter?               *'
           DISPLAY
           '*                                                   *'
           DISPLAY
           '* 0 - Quitter                                       *'
           DISPLAY
           '*                                                   *'
           DISPLAY
           '* 1 - Visiteurs                                     *'
           DISPLAY
           '*                                                   *'
           DISPLAY
           '* 2 - Actions                                       *'
           DISPLAY
           '*                                                   *'
           DISPLAY
           '* 3 - Arbitres                                      *'
           DISPLAY
           '*                                                   *'
           DISPLAY
           '* 4 - Joueurs                                       *'
           DISPLAY
           '*                                                   *'
           DISPLAY
           '* 5 - Equipes                                       *'
           DISPLAY
           '*                                                   *'
           DISPLAY
           '* 6 - Stades                                        *'
           DISPLAY
           '*                                                   *'
           DISPLAY
           '* 7 - Rencontres                                    *'
           DISPLAY
           '*                                                   *'
           DISPLAY
           '* 8 - Se deconnecter                                *'
           DISPLAY
           '*****************************************************'

           ACCEPT WchoixAdmin
           EVALUATE WchoixAdmin
             WHEN 1
                  PERFORM INFO_VISITEURS
             WHEN 2
                  PERFORM MENU_ACTIONS
             WHEN 3
                  PERFORM MENU_ARBITRES
             WHEN 4
                  PERFORM MENU_JOUEURS
             WHEN 5
                   PERFORM MENU_EQUIPES
             WHEN 6
                   PERFORM MENU_STADES
             WHEN 7
                  PERFORM MENU_RENCONTRES
             WHEN 8
                  PERFORM MENU_PRINCIPAL
           END-EVALUATE
           END-PERFORM.

           ADMINISTRATEUR.
           OPEN INPUT fadministrateurs
           IF fadministrateurs_stat = 35 THEN
               OPEN OUTPUT fadministrateurs
               CLOSE fadministrateurs
           END-IF
           DISPLAY 'Veuillez saisir votre mot de passe '
           ACCEPT  fa_mdp
            IF fa_mdp = Wmdp THEN
               PERFORM  AFFICHAGE_ADMINISTRATEUR
            ELSE
           DISPLAY 'Le mot de passe saisie est incorrect'
           PERFORM MENU_PRINCIPAL
           END-IF
           STOP RUN.
           CLOSE fadministrateurs.
