       IDENTIFICATION DIVISION.
       PROGRAM-ID. GROUPE_cob.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT Fgroupe ASSIGN TO "groupes.dat"
           ORGANIZATION indexed
           ACCESS IS DYNAMIC
           record key gr_lettre
           FILE STATUS IS Fgroupe_stat.


       DATA DIVISION.
       FILE SECTION.
           FD Fgroupe.
           01 groupeTampon.
               02 gr_lettre PIC A.
               02 gr_eq1_nom PIC A(30).
               02 gr_eq2_nom PIC A(30).
               02 gr_eq3_nom PIC A(30).
               02 gr_eq4_nom PIC A(30).
       WORKING-STORAGE SECTION.
           77 Wrep PIC 9(2).
           77 Fgroupe_stat PIC 9(2).
       77 Wfin PIC 9(2).
           77 choix PIC 9.
           77 choix_menu PIC 9.
           77 FIN_MENU PIC X(10).
       77 fin_fichier pic x(10).
           77 Wlettre_groupe pic A(10).
           77 Wgroupe_fin pic 9(2).
           77 gr_lettreC pic x(100).
           77 Wtrouve_groupe pic 9.
           77 Wfin_groupe pic 9.

       PROCEDURE DIVISION.

       OPEN I-O Fgroupe
       if Fgroupe_stat=35 THEN
           open output Fgroupe
           MOVE 'faux' TO FIN_MENU
           PERFORM MENU_PRINCIPAL
           UNTIL FIN_MENU='vrai'
       END-IF
       CLOSE Fgroupe.
       STOP RUN.

       MENU_PRINCIPAL.
      *PERFORM WITH TEST AFTER UNTIL choix = 0
        DISPLAY ' '
        DISPLAY '  ---------------------------------------  '
        DISPLAY '        GROUPES COUPE DU MONDE             '
        DISPLAY '  ---------------------------------------  '
        DISPLAY ' Indiquer le chiffre correspondant a  votre souhait ! '
        DISPLAY '  ---------------------------------------  '
        DISPLAY ' Quitter le programme : 0                  '
        DISPLAY '  ---------------------------------------  '
        DISPLAY ' Ajouter un groupe : 1               '
       DISPLAY '  ---------------------------------------  '
        DISPLAY ' Afficher un groupe : 2  '
        DISPLAY '  ---------------------------------------  '
        ACCEPT choix_menu
        EVALUATE choix_menu
        WHEN 1 PERFORM CREER_GROUPES
        WHEN 2 PERFORM AFFICHER-GROUPE
        WHEN 0 move 'vrai' to FIN_MENU
        WHEN OTHER DISPLAY "Entree fausse" choix_menu
        END-EVALUATE.

       CREER_GROUPES.

               DISPLAY 'CREATION DU GROUPE'
               DISPLAY 'Quel groupe voulez vous creer ?'
               ACCEPT gr_lettre
               DISPLAY 'Entrez nom équipe 1'
               ACCEPT gr_eq1_nom
               DISPLAY 'Entrez nom équipe 2'
               ACCEPT gr_eq2_nom
               DISPLAY 'Entrez nom équipe 3'
               ACCEPT gr_eq3_nom
               DISPLAY 'Entrez nom équipe 4'
               ACCEPT gr_eq4_nom
               open I-O Fgroupe
               WRITE groupeTampon END-WRITE
               close Fgroupe
               PERFORM WITH TEST AFTER UNTIL Wrep = 0 OR Wrep = 1
               DISPLAY 'Souhaitez vous continuer ? 1 ou 0'
                   ACCEPT Wrep
               END-PERFORM.

       AFFICHER-GROUPE.
        OPEN INPUT Fgroupe
        MOVE 0 TO Wfin_groupe
        MOVE 0 TO Wtrouve_groupe
        DISPLAY "groupe ?"
        ACCEPT Wlettre_groupe
        PERFORM WITH TEST AFTER UNTIL Wtrouve_groupe = 1 OR Wfin_groupe = 1
            READ Fgroupe NEXT

            AT END MOVE 1 TO Wfin_groupe
            DISPLAY 'Aucun groupe de ce nom !'

            NOT AT END
            IF Wlettre_groupe = gr_lettre THEN
                    MOVE 1 TO Wtrouve_groupe
                    display "Groupe : "gr_lettre
                    DISPLAY gr_eq1_nom
                   DISPLAY gr_eq2_nom
                   DISPLAY gr_eq3_nom
                   DISPLAY gr_eq4_nom

            END-IF
            END-READ
        END-PERFORM
        CLOSE Fgroupe.
       END PROGRAM GROUPE_cob.
