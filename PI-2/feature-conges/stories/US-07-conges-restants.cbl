       IDENTIFICATION DIVISION.
       PROGRAM-ID. US-07-CONGES-RESTANTS.
      * US-07 : Calculer les conges restants
      * Feature 3 : Gestion des conges
      * PI-2 : Systeme de Gestion des Employes

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FICHIER-CONGES
               ASSIGN TO '../conges.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT FICHIER-EMPLOYES
               ASSIGN TO '../../../PI-1/feature-paie/employes.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
        FD FICHIER-CONGES.
           COPY '../../../copybooks/conge.cpy'.

        FD FICHIER-EMPLOYES.
           COPY '../../../copybooks/employe.cpy'.

       WORKING-STORAGE SECTION.
        01 WS-FIN             PIC X     VALUE 'N'.
        01 WS-CP-TOTAL        PIC 9(3)  VALUE 25.
        01 WS-RTT-TOTAL       PIC 9(3)  VALUE 10.

        01 WS-TAB-EMPLOYES.
               05 WS-EMP OCCURS 5 TIMES.
                   10 WS-EMP-ID      PIC 9(5).
                   10 WS-EMP-NOM     PIC X(20).
                   10 WS-CP-PRIS     PIC 9(3) VALUE 0.
                   10 WS-RTT-PRIS    PIC 9(3) VALUE 0.

        01 WS-IDX             PIC 9(2)  VALUE 0.
        01 WS-IDX2            PIC 9(2)  VALUE 0.
        01 WS-AFF-NB          PIC ZZ9.

       PROCEDURE DIVISION.

           PERFORM CHARGER-EMPLOYES
           PERFORM CALCULER-CONGES
           PERFORM AFFICHER-RESULTATS
           STOP RUN.

       CHARGER-EMPLOYES.
           OPEN INPUT FICHIER-EMPLOYES
           MOVE 0 TO WS-IDX
           MOVE 'N' TO WS-FIN
           PERFORM UNTIL WS-FIN = 'O'
               READ FICHIER-EMPLOYES
                   AT END MOVE 'O' TO WS-FIN
                   NOT AT END
                       ADD 1 TO WS-IDX
                       MOVE EMP-ID TO WS-EMP-ID(WS-IDX)
                       MOVE EMP-NOM TO WS-EMP-NOM(WS-IDX)
               END-READ
           END-PERFORM
           CLOSE FICHIER-EMPLOYES.

       CALCULER-CONGES.
           OPEN INPUT FICHIER-CONGES
           MOVE 'N' TO WS-FIN
           PERFORM UNTIL WS-FIN = 'O'
               READ FICHIER-CONGES
                   AT END MOVE 'O' TO WS-FIN
                   NOT AT END
                       PERFORM VARYING WS-IDX2 FROM 1 BY 1
                           UNTIL WS-IDX2 > 5
                           IF CONGE-EMP-ID = WS-EMP-ID(WS-IDX2)
                               EVALUATE CONGE-TYPE
                                   WHEN 'CP        '
                                       ADD CONGE-NB-JOURS TO
                                           WS-CP-PRIS(WS-IDX2)
                                   WHEN 'RTT       '
                                       ADD CONGE-NB-JOURS TO
                                           WS-RTT-PRIS(WS-IDX2)
                               END-EVALUATE
                           END-IF
                       END-PERFORM
               END-READ
           END-PERFORM
           CLOSE FICHIER-CONGES.

       AFFICHER-RESULTATS.
           DISPLAY '================================'
           DISPLAY ' CONGES RESTANTS PAR EMPLOYE    '
           DISPLAY '================================'
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > 5
               DISPLAY '--------------------------------'
               DISPLAY 'Employe : ' WS-EMP-NOM(WS-IDX)
               MOVE WS-CP-PRIS(WS-IDX) TO WS-AFF-NB
               DISPLAY 'CP pris      : ' WS-AFF-NB
               SUBTRACT WS-CP-PRIS(WS-IDX) FROM WS-CP-TOTAL
                   GIVING WS-AFF-NB
               DISPLAY 'CP restants  : ' WS-AFF-NB
               MOVE WS-RTT-PRIS(WS-IDX) TO WS-AFF-NB
               DISPLAY 'RTT pris     : ' WS-AFF-NB
               SUBTRACT WS-RTT-PRIS(WS-IDX) FROM WS-RTT-TOTAL
                   GIVING WS-AFF-NB
               DISPLAY 'RTT restants : ' WS-AFF-NB
           END-PERFORM.