       IDENTIFICATION DIVISION.
       PROGRAM-ID. US-03-BULLETINS-PAIE.
      * US-03 : Ecrire les bulletins de paie
      * Feature : Calcul de la paie
      * PI-1 : Systeme de Gestion des Employes

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FICHIER-EMPLOYES
               ASSIGN TO '../employes.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT FICHIER-BULLETINS
               ASSIGN TO '../bulletins.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
        FD FICHIER-EMPLOYES.
           COPY '../../../copybooks/employe.cpy'.

        FD FICHIER-BULLETINS.
        01 LIGNE-BULLETIN     PIC X(80).

       WORKING-STORAGE SECTION.
        01 WS-FIN             PIC X         VALUE 'N'.
        01 WS-COTISATIONS     PIC 9(5)V9(2) VALUE 0.
        01 WS-IMPOT           PIC 9(5)V9(2) VALUE 0.
        01 WS-SALAIRE-NET     PIC 9(5)V9(2) VALUE 0.
        01 WS-TAUX-IMPOT      PIC 9(3)V9(2) VALUE 0.
        01 WS-LIGNE           PIC X(80)     VALUE SPACES.
        01 WS-BRUT-AFF        PIC ZZ999.99.
        01 WS-COTIS-AFF       PIC ZZ999.99.
        01 WS-IMPOT-AFF       PIC ZZ999.99.
        01 WS-NET-AFF         PIC ZZ999.99.

       PROCEDURE DIVISION.

           OPEN INPUT  FICHIER-EMPLOYES
           OPEN OUTPUT FICHIER-BULLETINS

           PERFORM UNTIL WS-FIN = 'O'
               READ FICHIER-EMPLOYES
                   AT END MOVE 'O' TO WS-FIN
                   NOT AT END
                       PERFORM CALCULER-SALAIRE
                       PERFORM ECRIRE-BULLETIN
               END-READ
           END-PERFORM.

           CLOSE FICHIER-EMPLOYES
           CLOSE FICHIER-BULLETINS
           DISPLAY 'Bulletins de paie generes : bulletins.txt'
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
               EMP-SALAIRE - WS-COTISATIONS - WS-IMPOT.

       ECRIRE-BULLETIN.
           MOVE '======================================' 
                       TO LIGNE-BULLETIN
           WRITE LIGNE-BULLETIN

           MOVE SPACES TO LIGNE-BULLETIN
           STRING 'BULLETIN DE PAIE - ' EMP-NOM ' ' EMP-PRENOM
               DELIMITED SIZE INTO LIGNE-BULLETIN
           WRITE LIGNE-BULLETIN

           MOVE SPACES TO LIGNE-BULLETIN
           STRING 'Categorie     : ' EMP-CATEGORIE
               DELIMITED SIZE INTO LIGNE-BULLETIN
           WRITE LIGNE-BULLETIN

           MOVE EMP-SALAIRE    TO WS-BRUT-AFF
           MOVE WS-COTISATIONS TO WS-COTIS-AFF
           MOVE WS-IMPOT       TO WS-IMPOT-AFF
           MOVE WS-SALAIRE-NET TO WS-NET-AFF

           MOVE SPACES TO LIGNE-BULLETIN
           STRING 'Salaire Brut  : ' WS-BRUT-AFF
               DELIMITED SIZE INTO LIGNE-BULLETIN
           WRITE LIGNE-BULLETIN

           MOVE SPACES TO LIGNE-BULLETIN
           STRING 'Cotisations   : ' WS-COTIS-AFF
               DELIMITED SIZE INTO LIGNE-BULLETIN
           WRITE LIGNE-BULLETIN

           MOVE SPACES TO LIGNE-BULLETIN
           STRING 'Impot         : ' WS-IMPOT-AFF
               DELIMITED SIZE INTO LIGNE-BULLETIN
           WRITE LIGNE-BULLETIN

           MOVE SPACES TO LIGNE-BULLETIN
           STRING 'Salaire Net   : ' WS-NET-AFF
               DELIMITED SIZE INTO LIGNE-BULLETIN
           WRITE LIGNE-BULLETIN.
