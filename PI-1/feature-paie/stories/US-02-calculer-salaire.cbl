       IDENTIFICATION DIVISION.
       PROGRAM-ID. US-02-CALCULER-SALAIRE.
      * US-02 : Calculer le salaire net
      * Feature : Calcul de la paie
      * PI-1 : Systeme de Gestion des Employes

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FICHIER-EMPLOYES
               ASSIGN TO '../employes.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
        FD FICHIER-EMPLOYES.
           COPY '../../../copybooks/employe.cpy'.

       WORKING-STORAGE SECTION.
        01 WS-FIN             PIC X     VALUE 'N'.
        01 WS-COTISATIONS     PIC 9(5)V9(2) VALUE 0.
        01 WS-IMPOT           PIC 9(5)V9(2) VALUE 0.
        01 WS-SALAIRE-NET     PIC 9(5)V9(2) VALUE 0.
        01 WS-TAUX-IMPOT      PIC 9(3)V9(2) VALUE 0.

       PROCEDURE DIVISION.

           OPEN INPUT FICHIER-EMPLOYES

           DISPLAY '================================'
           DISPLAY ' CALCUL DES SALAIRES NETS       '
           DISPLAY '================================'

           PERFORM UNTIL WS-FIN = 'O'
               READ FICHIER-EMPLOYES
                   AT END MOVE 'O' TO WS-FIN
                   NOT AT END
                       PERFORM CALCULER-SALAIRE
               END-READ
           END-PERFORM.

           CLOSE FICHIER-EMPLOYES
           STOP RUN.

       CALCULER-SALAIRE.
           COMPUTE WS-COTISATIONS = EMP-SALAIRE * 0.22

           EVALUATE EMP-CATEGORIE
               WHEN 'CDI       '
                   MOVE 0.10 TO WS-TAUX-IMPOT
               WHEN 'CDD       '
                   MOVE 0.05 TO WS-TAUX-IMPOT
               WHEN OTHER
                   MOVE 0.00 TO WS-TAUX-IMPOT
           END-EVALUATE

           COMPUTE WS-IMPOT = EMP-SALAIRE * WS-TAUX-IMPOT
           COMPUTE WS-SALAIRE-NET =
               EMP-SALAIRE - WS-COTISATIONS - WS-IMPOT

           DISPLAY '--------------------------------'
           DISPLAY 'Employe  : ' EMP-NOM
           DISPLAY 'Categorie: ' EMP-CATEGORIE
           DISPLAY 'Brut     : ' EMP-SALAIRE
           DISPLAY 'Cotis.   : ' WS-COTISATIONS
           DISPLAY 'Impot    : ' WS-IMPOT
           DISPLAY 'NET      : ' WS-SALAIRE-NET.