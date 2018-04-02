           IDENTIFICATION DIVISION.
           PROGRAM-ID. ProjetCoupeDuMonde_cbl.
           ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.



           FILE-CONTROL.
           SELECT Fgroupe ASSIGN TO "groupes.dat"
           ORGANIZATION sequential
           ACCESS IS sequential
           FILE STATUS IS Fgroupe_stat.

           SELECT Fmatch ASSIGN TO "matchs.dat"
           ORGANIZATION indexed
           ACCESS IS sequential
           RECORD KEY m_id
           ALTERNATE RECORD KEY m_nomEquipe1 WITH DUPLICATES
           ALTERNATE RECORD KEY m_nomEquipe2 WITH DUPLICATES
           ALTERNATE RECORD KEY m_statut WITH DUPLICATES
           FILE STATUS IS Fmatch_stat.

           SELECT FstatMatch ASSIGN TO "statsMatchs.dat"
           ORGANIZATION indexed
           ACCESS IS sequential
           RECORD KEY stat_m_id
           ALTERNATE RECORD KEY stat_m_scoreEq1
           ALTERNATE RECORD KEY stat_m_scoreEq2
           FILE STATUS IS FstatMatch_stat.

           SELECT Fequipe ASSIGN TO "equipes.dat"
           ORGANIZATION indexed
           ACCESS IS sequential
           RECORD KEY eq_nom
           ALTERNATE RECORD KEY eq_nomGroupe WITH DUPLICATES
           FILE STATUS IS Fequipe_stat.

           DATA DIVISION.
           FILE SECTION.
           FD Fgroupe.
           01 groupeTampon.
               02 gr_lettre PIC A(1).

           FD Fmatch.
           01 matchTampon.
               02 m_id PIC 9(10).
               02 m_statut PIC A(10).
               02 m_nomEquipe1 PIC 9(10).
               02 m_nomEquipe2 PIC 9(10).
               02 m_nomStade PIC A(20).

           FD FstatMatch.
           01 statMatchTampon.
               02 stat_m_id PIC 9(10).
               02 stat_m_nbSpect PIC 9(10).
               02 stat_m_scoreEq1 PIC 9(10).
               02 stat_m_scoreEq2 PIC 9(10).
               02 stat_m_note PIC 9(10).

           FD Fequipe.
           01 equipeTampon.
               02 eq_nom PIC A(30).
               02 eq_nomGroupe PIC A(1).
               02 eq_nbPoints PIC 9(2).
               02 eq_nomSelectionneur PIC A(10).
               02 eq_sponsor PIC A(20).

           WORKING-STORAGE SECTION.
           77 Fequipe_stat PIC 9(2).
           77 FstatMatch_stat PIC 9(2).
           77 Fmatch_stat PIC 9(2).
           77 Fgroupe_stat PIC 9(2).
           77 Wrep PIC 9(2).
           77 TampoDernierMatch PIC 9(10).
           77 Wfini PIC 9.
           77 Wtrouver PIC 9.
           77 TampoIdMatch PIC 9(10).
           77 TampoNomEquipe PIC A(30).
           77 TampoNbPoints PIC 9(2).
           77 TampoNomSelect PIC A(10).
           77 TampoSponsor PIC A(20).
           77 TampoNomGroupe PIC A(1).
           77 NbEquipeGroupe PIC 9(2).
           77 Ok PIC 9.
           77 TampoGroupeLettre Pic A(1).
           77 NbGroupes PIC 9(1).
           77 Wchoix PIC 9.


           PROCEDURE DIVISION.
           OPEN EXTEND Fgroupe
           IF groupeTampon = 35 THEN
               OPEN OUTPUT Fgroupe
           END-IF
           CLOSE Fgroupe

           OPEN I-O Fmatch
           IF matchTampon = 35 THEN
               OPEN OUTPUT Fmatch
           END-IF
           CLOSE Fmatch

           OPEN I-O FstatMatch
           IF statMatchTampon = 35 THEN
               OPEN OUTPUT FstatMatch
           END-IF
           CLOSE FstatMatch

           OPEN I-O Fequipe
           IF equipeTampon = 35 THEN
           OPEN OUTPUT Fequipe
           END-IF
           CLOSE Fequipe

           PERFORM WITH TEST AFTER UNTIL Wchoix = 0
               DISPLAY'QUELLE ACTION VOULEZ VOUS FAIRE'
               DISPLAY'1 - AJOUTER UN GROUPE'
               DISPLAY'2 - AJOUTER UNE EQUIPE '
               DISPLAY'3 - AFFICHER UN GROUPE'
               DISPLAY'4 - AFFICHER UNE EQUIPE'
               ACCEPT Wchoix
               EVALUATE Wchoix
                   WHEN 1
                       PERFORM AJOUTER_GROUPE
                   WHEN 2
                       PERFORM AJOUTER_EQUIPE
                   WHEN 3
                       PERFORM AFFICHER_TOUS_GROUPE
                   WHEN 4
                       PERFORM AFFICHER_TOUTES_EQUIPE
                   WHEN OTHER
                       MOVE 0 TO Wchoix
               STOP RUN
           END-PERFORM.



           AUTO_INCREMENT_ID_MATCH.
               OPEN INPUT Fmatch
               MOVE 0 TO Wfini
               PERFORM WITH TEST AFTER UNTIL Wfini = 1
                   READ Fmatch
                   AT END MOVE 1 TO Wfini
                   NOT at END
                       MOVE m_id TO TampoDernierMatch
               END-PERFORM
           CLOSE Fmatch.

           EXISTE_EQUIPE.
               OPEN INPUT Fequipe
               MOVE 0 TO Wfini
               MOVE 0 TO Wtrouver
               PERFORM WITH TEST AFTER UNTIL Wtrouver = 1 OR Wfini = 1
                   READ Fequipe
                   AT END MOVE 1 TO Wfini
                   NOT at END
                   IF eq_nom = TampoNomEquipe THEN
                     MOVE 1 TO Wtrouver
                   END-IF
               END-PERFORM
               CLOSE Fequipe.

           EXISTE_GROUPE.
               OPEN INPUT Fgroupe
               MOVE 0 TO Wfini
               MOVE 0 TO Wtrouver
               PERFORM WITH TEST AFTER UNTIL Wtrouver = 1 OR Wfini = 1
                   READ Fgroupe
                   AT END MOVE 1 TO Wfini
                   NOT at END
                   IF gr_lettre = TampoNomGroupe THEN
                     MOVE 1 TO Wtrouver
                   END-IF
               END-PERFORM
               CLOSE Fgroupe.

           NB_EQUIPE_GROUPE.
               OPEN INPUT Fequipe
               MOVE 0 TO Wfini
               MOVE 0 TO NbEquipeGroupe
               MOVE TampoNomGroupe TO eq_nomGroupe
                   START Fequipe KEY IS = eq_nomGroupe
                   INVALID KEY
                       DISPLAY'Groupe innexistant'
                   NOT INVALID KEY
                       PERFORM WITH TEST AFTER UNTIL Wfini = 1
                           READ Fequipe NEXT
                           AT END
                           MOVE 1 TO Wfini
                           NOT AT END
                            IF eq_nomGroupe = TampoNomGroupe THEN
                             COMPUTE NbEquipeGroupe = NbEquipeGroupe + 1
                            END-IF
                           END-READ
                       END-PERFORM
                   END-START
               CLOSE Fequipe.


           AJOUTER_EQUIPE.
           PERFORM WITH TEST AFTER UNTIL Wrep = 0
            DISPLAY'AJOUTEZ UNe EQUIPE EN AJOUTANT LES INFORMATIONS'
            DISPLAY'NOM EQUIPE : '
            ACCEPT TampoNomEquipe;


            DISPLAY'saisir le nom du groupe auquel on veut l assigner'
            ACCEPT TampoNomGroupe
            PERFORM EXISTE_GROUPE
            PERFORM WITH TEST AFTER UNTIL Wtrouver = 1
                   DISPLAY'Le groupe n existe pas'
                   DISPLAY'Saisir un nom de groupe existant'
                   ACCEPT TampoNomGroupe
                   PERFORM EXISTE_GROUPE
            END-PERFORM
            PERFORM NB_EQUIPE_GROUPE
            PERFORM WITH TEST AFTER UNTIL NbEquipeGroupe IS < 2
               DISPLAY'Cet groupe est deja complet'
               DISPLAY'saisir un autre groupe'
               ACCEPT TampoNomGroupe
               PERFORM NB_EQUIPE_GROUPE
            END-PERFORM
            MOVE 0 TO TampoNbPoints
            DISPLAY'Saisir le nom du selectionneur : '
            ACCEPT TampoNomSelect
            DISPLAY'Saisir le sponsor de lequipe :'
            ACCEPT TampoSponsor
            OPEN EXTEND Fequipe
            MOVE TampoNomEquipe TO eq_nom
            MOVE TampoNomGroupe TO eq_nomGroupe
            MOVE TampoNbPoints TO eq_nbPoints
            MOVE TampoNomSelect TO eq_nomSelectionneur
            MOVE TampoSponsor TO eq_sponsor
            WRITE equipeTampon END-WRITE
            PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
             DISPLAY'souhaitez vous continuer? 1 ou 0'
             ACCEPT Wrep
            END-PERFORM
           END-PERFORM
           CLOSE Fequipe.

           AJOUTER_GROUPE.
           PERFORM WITH TEST AFTER UNTIL Wrep = 0
               DISPLAY'Saisir la lettre correspondant au nom du groupe'
               IF NbGroupes IS  < 8 THEN
                   ACCEPT TampoGroupeLettre
                   OPEN EXTEND Fgroupe
                   MOVE TampoGroupeLettre TO gr_lettre
                   WRITE groupeTampon END-WRITE
               ELSE
                   DISPLAY'le nombre max de groupe est deja atteint'
               END-IF
               PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
                   DISPLAY'Souhaitez vous continuer? 1 ou 0'
                   ACCEPT Wrep
               END-PERFORM
           END-PERFORM
           CLOSE Fgroupe.

           AFFICHER_TOUS_GROUPE.
           MOVE 0 TO Wfini
           OPEN INPUT Fgroupe
           PERFORM WITH TEST AFTER UNTIL Wfini = 1
               READ Fgroupe NEXT
               AT END
                   MOVE 1 TO Wfini
               NOT AT END
                   DISPLAY "Groupe : "gr_lettre
               END-READ
           END-PERFORM
           CLOSE Fgroupe.

           AFFICHER_TOUTES_EQUIPE.
           MOVE 0 TO Wfini
           OPEN INPUT Fequipe
           PERFORM WITH TEST AFTER UNTIL Wfini = 1
               READ Fequipe
               AT END
                   MOVE 1 TO Wfini
               NOT AT END
                   DISPLAY "EQUIPE : "eq_nom
               END-READ
           END-PERFORM
           CLOSE Fequipe.


           CREER_MATCH_POULES.
           PERFORM WITH TEST AFTER UNTIL Wrep=0
               DISPLAY 'Donnez les informations du match de poules'
               PERFORM AUTO_INCREMENT_ID_MATCH
               COMPUTE TampoIdMatch = TampoDernierMatch + 1
               MOVE 'poules' TO m_statut
               DISPLAY 'nom equipe 1'
               ACCEPT m_nomEquipe1
               DISPLAY 'nom equipe 2'
               ACCEPT m_nomEquipe2
               DISPLAY 'nom stade'
               ACCEPT m_nomStade
               WRITE matchTampon END-WRITE
               PERFORM WITH TEST AFTER UNTIL Wrep=0 OR Wrep=1
                   DISPLAY 'Souhaitez vous continuer ? 1 ou 0'
                   ACCEPT Wrep
               END-PERFORM
           END-PERFORM.


           END PROGRAM ProjetCoupeDuMonde_cbl.
