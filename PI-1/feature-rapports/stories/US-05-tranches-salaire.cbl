       IDENTIFICATION DIVISION.
       PROGRAM-ID. US-05-TRANCHES-SALAIRE.
      * US-05 : Rapport par tranche de salaire
      * Feature : Rapports
      * PI-1 : Systeme de Gestion des Employes

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FICHIER-EMPLOYES
               ASSIGN TO '../../feature-paie/employes.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT FICHIER-RAPPORT
               ASSIGN TO '../rapport-tranches.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
        FD FICHIER-EMPLOYES.
           COPY '../../../copybooks/employe.cpy'.

        FD FICHIER-RAPPORT.
        01 LIGNE-RAPPORT      PIC X(80).

       WORKING-STORAGE SECTION.
        01 WS-FIN             PIC X         VALUE 'N'.
        01 WS-NB-TRANCHE1     PIC 9(3)      VALUE 0.
        01 WS-NB-TRANCHE2     PIC 9(3)      VALUE 0.
        01 WS-NB-TRANCHE3     PIC 9(3)      VALUE 0.
        01 WS-TOT-TRANCHE1    PIC 9(7)V9(2) VALUE 0.
        01 WS-TOT-TRANCHE2    PIC 9(7)V9(2) VALUE 0.
        01 WS-TOT-TRANCHE3    PIC 9(7)V9(2) VALUE 0.
        01 WS-AFF             PIC ZZZ999.99.
        01 WS-AFF-NB          PIC ZZ9.
        01 WS-LIGNE           PIC X(80)     VALUE SPACES.

       PROCEDURE DIVISION.

           OPEN INPUT  FICHIER-EMPLOYES
           OPEN OUTPUT FICHIER-RAPPORT

           PERFORM UNTIL WS-FIN = 'O'
               READ FICHIER-EMPLOYES
                   AT END MOVE 'O' TO WS-FIN
                   NOT AT END
                       PERFORM CLASSER-TRANCHE
               END-READ
           END-PERFORM.

           PERFORM ECRIRE-RAPPORT

           CLOSE FICHIER-EMPLOYES
           CLOSE FICHIER-RAPPORT
           DISPLAY 'Rapport genere : rapport-tranches.txt'
           STOP RUN.

       CLASSER-TRANCHE.
           EVALUATE TRUE
               WHEN EMP-SALAIRE < 2000
                   ADD 1 TO WS-NB-TRANCHE1
                   ADD EMP-SALAIRE TO WS-TOT-TRANCHE1
               WHEN EMP-SALAIRE < 3000
                   ADD 1 TO WS-NB-TRANCHE2
                   ADD EMP-SALAIRE TO WS-TOT-TRANCHE2
               WHEN OTHER
                   ADD 1 TO WS-NB-TRANCHE3
                   ADD EMP-SALAIRE TO WS-TOT-TRANCHE3
           END-EVALUATE.

       ECRIRE-RAPPORT.
           MOVE '======================================' 
                TO LIGNE-RAPPORT
           WRITE LIGNE-RAPPORT
           MOVE 'RAPPORT PAR TRANCHE DE SALAIRE' TO LIGNE-RAPPORT
           WRITE LIGNE-RAPPORT
           MOVE '======================================' 
                TO LIGNE-RAPPORT
           WRITE LIGNE-RAPPORT

           MOVE 'Tranche 1 : moins de 2000 EUR' TO LIGNE-RAPPORT
           WRITE LIGNE-RAPPORT
           MOVE WS-NB-TRANCHE1 TO WS-AFF-NB
           MOVE SPACES TO LIGNE-RAPPORT
           STRING '  Nombre    : ' WS-AFF-NB
               DELIMITED SIZE INTO LIGNE-RAPPORT
           WRITE LIGNE-RAPPORT
           MOVE WS-TOT-TRANCHE1 TO WS-AFF
           MOVE SPACES TO LIGNE-RAPPORT
           STRING '  Total     : ' WS-AFF
               DELIMITED SIZE INTO LIGNE-RAPPORT
           WRITE LIGNE-RAPPORT

           MOVE '--------------------------------------' 
                TO LIGNE-RAPPORT
           WRITE LIGNE-RAPPORT

           MOVE 'Tranche 2 : entre 2000 et 3000 EUR' TO LIGNE-RAPPORT
           WRITE LIGNE-RAPPORT
           MOVE WS-NB-TRANCHE2 TO WS-AFF-NB
           MOVE SPACES TO LIGNE-RAPPORT
           STRING '  Nombre    : ' WS-AFF-NB
               DELIMITED SIZE INTO LIGNE-RAPPORT
           WRITE LIGNE-RAPPORT
           MOVE WS-TOT-TRANCHE2 TO WS-AFF
           MOVE SPACES TO LIGNE-RAPPORT
           STRING '  Total     : ' WS-AFF
               DELIMITED SIZE INTO LIGNE-RAPPORT
           WRITE LIGNE-RAPPORT

           MOVE '--------------------------------------' 
                TO LIGNE-RAPPORT
           WRITE LIGNE-RAPPORT

           MOVE 'Tranche 3 : plus de 3000 EUR' TO LIGNE-RAPPORT
           WRITE LIGNE-RAPPORT
           MOVE WS-NB-TRANCHE3 TO WS-AFF-NB
           MOVE SPACES TO LIGNE-RAPPORT
           STRING '  Nombre    : ' WS-AFF-NB
               DELIMITED SIZE INTO LIGNE-RAPPORT
           WRITE LIGNE-RAPPORT
           MOVE WS-TOT-TRANCHE3 TO WS-AFF
           MOVE SPACES TO LIGNE-RAPPORT
           STRING '  Total     : ' WS-AFF
               DELIMITED SIZE INTO LIGNE-RAPPORT
           WRITE LIGNE-RAPPORT

           MOVE '======================================' 
                TO LIGNE-RAPPORT
           WRITE LIGNE-RAPPORT.