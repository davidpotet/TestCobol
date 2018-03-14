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
               02 gr_lettre PIC A.

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
               02 eq_nom PIC 9(10).
               02 eq_nomGroupe PIC A(10).
               02 eq_nbPoints PIC 9(10).
               02 eq_nomSelectionneur PIC 9(10).
               02 eq_sponsor PIC A(20).

           WORKING-STORAGE SECTION.
           77 Fequipe_stat PIC 9(2).
           77 FstatMatch_stat PIC 9(2).
           77 Fmatch_stat PIC 9(2).
           77 Fgroupe_stat PIC 9(2).
           77 Wrep PIC 9(2).


           PROCEDURE DIVISION.


           OPEN EXTEND Fequipe
           IF Fequipe_stat =35 THEN
           OPEN OUTPUT Fequipe
           CLOSE Fequipe
           OPEN I-O Fequipe
           END-IF
           CLOSE Fequipe



           AJOUTER_EQUIPE.
           OPEN I-O Fequipe



           CREER_MATCH_POULES.
           PERFORM WITH TEST AFTER UNTIL Wrep=0
               DISPLAY 'Donnez les informations du match de poules'
               DISPLAY 'id'
               ACCEPT m_id
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