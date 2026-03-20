       IDENTIFICATION DIVISION.
       PROGRAM-ID. US-04-MASSE-SALARIALE.
      * US-04 : Rapport masse salariale totale
      * Feature : Rapports
      * PI-1 : Systeme de Gestion des Employes

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FICHIER-EMPLOYES
               ASSIGN TO '../../feature-paie/employes.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT FICHIER-RAPPORT
               ASSIGN TO '../rapport-masse-salariale.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
        FD FICHIER-EMPLOYES.
           COPY '../../../copybooks/employe.cpy'.

        FD FICHIER-RAPPORT.
        01 LIGNE-RAPPORT      PIC X(80). 

       WORKING-STORAGE SECTION.
        01 WS-FIN             PIC X         VALUE 'N'.
        01 WS-TOTAL-BRUT      PIC 9(7)V9(2) VALUE 0.
        01 WS-TOTAL-COTIS     PIC 9(7)V9(2) VALUE 0.
        01 WS-TOTAL-IMPOT     PIC 9(7)V9(2) VALUE 0.
        01 WS-TOTAL-NET       PIC 9(7)V9(2) VALUE 0.
        01 WS-NB-EMPLOYES     PIC 9(3)      VALUE 0.
        01 WS-NB-CDI          PIC 9(3)      VALUE 0.
        01 WS-NB-CDD          PIC 9(3)      VALUE 0.
        01 WS-NB-STAGE        PIC 9(3)      VALUE 0.
        01 WS-COTISATIONS     PIC 9(5)V9(2) VALUE 0.
        01 WS-IMPOT           PIC 9(5)V9(2) VALUE 0.
        01 WS-SALAIRE-NET     PIC 9(5)V9(2) VALUE 0.
        01 WS-TAUX-IMPOT      PIC 9(3)V9(2) VALUE 0.
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
                       PERFORM CALCULER-EMPLOYE
               END-READ
           END-PERFORM.

           PERFORM ECRIRE-RAPPORT

           CLOSE FICHIER-EMPLOYES
           CLOSE FICHIER-RAPPORT
           DISPLAY 'Rapport genere : rapport-masse-salariale.txt'
           STOP RUN.

       CALCULER-EMPLOYE.
           ADD 1 TO WS-NB-EMPLOYES
           COMPUTE WS-COTISATIONS = EMP-SALAIRE * 0.22

           EVALUATE EMP-CATEGORIE
               WHEN 'CDI       '
                   MOVE 0.10 TO WS-TAUX-IMPOT
                   ADD 1 TO WS-NB-CDI
               WHEN 'CDD       '
                   MOVE 0.05 TO WS-TAUX-IMPOT
                   ADD 1 TO WS-NB-CDD
               WHEN OTHER
                   MOVE 0.00 TO WS-TAUX-IMPOT
                   ADD 1 TO WS-NB-STAGE
           END-EVALUATE

           COMPUTE WS-IMPOT = EMP-SALAIRE * WS-TAUX-IMPOT
           COMPUTE WS-SALAIRE-NET =
               EMP-SALAIRE - WS-COTISATIONS - WS-IMPOT

           ADD EMP-SALAIRE    TO WS-TOTAL-BRUT
           ADD WS-COTISATIONS TO WS-TOTAL-COTIS
           ADD WS-IMPOT       TO WS-TOTAL-IMPOT
           ADD WS-SALAIRE-NET TO WS-TOTAL-NET.

       ECRIRE-RAPPORT.
           MOVE '======================================' 
                              TO LIGNE-RAPPORT
           WRITE LIGNE-RAPPORT
           MOVE 'RAPPORT MASSE SALARIALE' TO LIGNE-RAPPORT
           WRITE LIGNE-RAPPORT
           MOVE '======================================' 
                              TO LIGNE-RAPPORT
           WRITE LIGNE-RAPPORT

           MOVE WS-NB-EMPLOYES TO WS-AFF-NB
           MOVE SPACES TO LIGNE-RAPPORT
           STRING 'Nombre employes : ' WS-AFF-NB
               DELIMITED SIZE INTO LIGNE-RAPPORT
           WRITE LIGNE-RAPPORT

           MOVE WS-NB-CDI TO WS-AFF-NB
           MOVE SPACES TO LIGNE-RAPPORT
           STRING '  CDI   : ' WS-AFF-NB
               DELIMITED SIZE INTO LIGNE-RAPPORT
           WRITE LIGNE-RAPPORT

           MOVE WS-NB-CDD TO WS-AFF-NB
           MOVE SPACES TO LIGNE-RAPPORT
           STRING '  CDD   : ' WS-AFF-NB
               DELIMITED SIZE INTO LIGNE-RAPPORT
           WRITE LIGNE-RAPPORT

           MOVE WS-NB-STAGE TO WS-AFF-NB
           MOVE SPACES TO LIGNE-RAPPORT
           STRING '  STAGE : ' WS-AFF-NB
               DELIMITED SIZE INTO LIGNE-RAPPORT
           WRITE LIGNE-RAPPORT

           MOVE '--------------------------------------' 
                              TO LIGNE-RAPPORT
           WRITE LIGNE-RAPPORT

           MOVE WS-TOTAL-BRUT TO WS-AFF
           MOVE SPACES TO LIGNE-RAPPORT
           STRING 'Total Brut      : ' WS-AFF
               DELIMITED SIZE INTO LIGNE-RAPPORT
           WRITE LIGNE-RAPPORT

           MOVE WS-TOTAL-COTIS TO WS-AFF
           MOVE SPACES TO LIGNE-RAPPORT
           STRING 'Total Cotis.    : ' WS-AFF
               DELIMITED SIZE INTO LIGNE-RAPPORT
           WRITE LIGNE-RAPPORT

           MOVE WS-TOTAL-IMPOT TO WS-AFF
           MOVE SPACES TO LIGNE-RAPPORT
           STRING 'Total Impot     : ' WS-AFF
               DELIMITED SIZE INTO LIGNE-RAPPORT
           WRITE LIGNE-RAPPORT

           MOVE WS-TOTAL-NET TO WS-AFF
           MOVE SPACES TO LIGNE-RAPPORT
           STRING 'Total Net       : ' WS-AFF
               DELIMITED SIZE INTO LIGNE-RAPPORT
           WRITE LIGNE-RAPPORT

           MOVE '======================================' 
                              TO LIGNE-RAPPORT
           WRITE LIGNE-RAPPORT.