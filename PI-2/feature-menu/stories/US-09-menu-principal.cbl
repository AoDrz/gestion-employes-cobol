       IDENTIFICATION DIVISION.
       PROGRAM-ID. US-09-MENU-PRINCIPAL.
      * US-09 : Afficher le menu principal
      * US-10 : Naviguer entre les fonctions
      * Feature 4 : Menu interactif
      * PI-2 : Systeme de Gestion des Employes

       DATA DIVISION.
       WORKING-STORAGE SECTION.
        01 WS-CHOIX         PIC X VALUE SPACES.
        01 WS-CONTINUER     PIC X VALUE 'O'.

       PROCEDURE DIVISION.

           PERFORM UNTIL WS-CONTINUER = 'N'
               PERFORM AFFICHER-MENU
               PERFORM TRAITER-CHOIX
           END-PERFORM.

           DISPLAY '================================'
           DISPLAY ' Au revoir !                    '
           DISPLAY '================================'
           STOP RUN.

       AFFICHER-MENU.
           DISPLAY ' '
           DISPLAY '================================'
           DISPLAY ' SYSTEME DE GESTION EMPLOYES    '
           DISPLAY '================================'
           DISPLAY ' 1. Calcul de la paie           '
           DISPLAY ' 2. Gestion des conges          '
           DISPLAY ' 3. Rapport masse salariale     '
           DISPLAY ' 4. Rapport tranches salaires   '
           DISPLAY ' 5. Rapport conges              '
           DISPLAY ' 6. Quitter                     '
           DISPLAY '================================'
           DISPLAY ' Votre choix : '
           ACCEPT WS-CHOIX.

       TRAITER-CHOIX.
           EVALUATE WS-CHOIX
               WHEN '1'
                   DISPLAY ' -> Calcul paie lance'
               WHEN '2'
                   DISPLAY ' -> Gestion conges lancee'
               WHEN '3'
                   DISPLAY ' -> Rapport masse salariale lance'
               WHEN '4'
                   DISPLAY ' -> Rapport tranches lance'
               WHEN '5'
                   DISPLAY ' -> Rapport conges lance'
               WHEN '6'
                   MOVE 'N' TO WS-CONTINUER
               WHEN OTHER
                   DISPLAY ' -> Choix invalide ! Saisir 1 a 6'
                   
           END-EVALUATE.