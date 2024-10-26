      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 TABULEIRO.
           03 LINHA-1                OCCURS 3 TIMES.
           05 LINHA-2          PIC X OCCURS 3 TIMES VALUE "_".
       77 POSICAO              PIC 9V99.
       77 JOGADOR              PIC X                VALUE "X".
       77 CONTADOR             PIC 9                VALUE 0.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           DISPLAY "INSIRA UMA OPCAO ENTRE 1.1 E 3.3"
           DISPLAY " "
           DISPLAY LINHA-2(1,1) "|" LINHA-2(1,2) "|" LINHA-2(1,3)
           DISPLAY LINHA-2(2,1) "|" LINHA-2(2,2) "|" LINHA-2(2,3)
           DISPLAY LINHA-2(3,1) "|" LINHA-2(3,2) "|" LINHA-2(3,3).

           SEQUENCIA.
           PERFORM JOGADA.
           PERFORM CHECK.
           PERFORM EXIBIRTABULEIRO.
           PERFORM ALTERNAR.
           PERFORM NOVAJOGADA.


           EXIBIRTABULEIRO.
             DISPLAY " "
             DISPLAY LINHA-2(1,1) "|" LINHA-2(1,2) "|" LINHA-2(1,3)
             DISPLAY LINHA-2(2,1) "|" LINHA-2(2,2) "|" LINHA-2(2,3)
             DISPLAY LINHA-2(3,1) "|" LINHA-2(3,2) "|" LINHA-2(3,3).

           JOGADA.
               DISPLAY "JOGADOR " JOGADOR ", ESCOLHA UMA POSICAO"
               ACCEPT POSICAO
                   EVALUATE POSICAO
                       WHEN 1.1
                          IF LINHA-2(1,1) = "_"
                               MOVE JOGADOR TO LINHA-2(1,1)
                          ELSE
                               DISPLAY "POSICAO OCUPADA"
                               PERFORM JOGADA
                       WHEN 1.2
                          IF LINHA-2(1,2) = "_"
                               MOVE JOGADOR TO LINHA-2(1,2)
                          ELSE
                               DISPLAY "POSICAO OCUPADA"
                               PERFORM JOGADA
                       WHEN 1.3
                          IF LINHA-2(1,3) = "_"
                               MOVE JOGADOR TO LINHA-2(1,3)
                          ELSE
                               DISPLAY "POSICAO OCUPADA"
                               PERFORM JOGADA
                       WHEN 2.1
                          IF LINHA-2(2,1) = "_"
                               MOVE JOGADOR TO LINHA-2(2,1)
                          ELSE
                               DISPLAY "POSICAO OCUPADA"
                               PERFORM JOGADA
                       WHEN 2.2
                          IF LINHA-2(2,2) = "_"
                               MOVE JOGADOR TO LINHA-2(2,2)
                          ELSE
                               DISPLAY "POSICAO OCUPADA"
                               PERFORM JOGADA
                       WHEN 2.3
                          IF LINHA-2(2,3) = "_"
                               MOVE JOGADOR TO LINHA-2(2,3)
                          ELSE
                               DISPLAY "POSICAO OCUPADA"
                               PERFORM JOGADA
                       WHEN 3.1
                          IF LINHA-2(3,1) = "_"
                               MOVE JOGADOR TO LINHA-2(3,1)
                          ELSE
                               DISPLAY "POSICAO OCUPADA"
                               PERFORM JOGADA
                       WHEN 3.2
                          IF LINHA-2(3,2) = "_"
                               MOVE JOGADOR TO LINHA-2(3,2)
                          ELSE
                               DISPLAY "POSICAO OCUPADA"
                               PERFORM JOGADA
                       WHEN 3.3
                          IF LINHA-2(3,3) = "_"
                               MOVE JOGADOR TO LINHA-2(3,3)
                          ELSE
                               DISPLAY "POSICAO OCUPADA"
                               PERFORM JOGADA
                       WHEN OTHER
                          DISPLAY "POSICAO INVALIDA"
                          PERFORM JOGADA
                   END-EVALUATE.

           ALTERNAR.
               IF JOGADOR = "X"
                   MOVE "O" TO JOGADOR
               ELSE
                   MOVE "X" TO JOGADOR
               END-IF.

               ADD 1 TO CONTADOR.

           CHECK.
               IF LINHA-2(1,1) = "X" AND LINHA-2(1,2) = "X" AND
                  LINHA-2(1,3) = "X"
                       PERFORM WIN
               END-IF.

               IF LINHA-2(2,1) = "X" AND LINHA-2(2,2) = "X" AND
                  LINHA-2(2,3) = "X"
                       PERFORM WIN
               END-IF.

               IF LINHA-2(3,1) = "X" AND LINHA-2(3,2) = "X" AND
                  LINHA-2(3,3) = "X"
                       PERFORM WIN
               END-IF.

               IF LINHA-2(1,1) = "X" AND LINHA-2(2,1) = "X" AND
                  LINHA-2(3,1) = "X"
                       PERFORM WIN
               END-IF.

               IF LINHA-2(1,2) = "X" AND LINHA-2(2,2) = "X" AND
                  LINHA-2(3,2) = "X"
                       PERFORM WIN
               END-IF.

               IF LINHA-2(1,3) = "X" AND LINHA-2(2,3) = "X" AND
                  LINHA-2(3,3) = "X"
                       PERFORM WIN
               END-IF.

               IF LINHA-2(1,3) = "X" AND LINHA-2(2,2) = "X" AND
                  LINHA-2(3,1) = "X"
                       PERFORM WIN
               END-IF.

               IF LINHA-2(1,1) = "X" AND LINHA-2(2,2) = "X" AND
                  LINHA-2(3,3) = "X"
                       PERFORM WIN
               END-IF.

               IF LINHA-2(1,1) = "O" AND LINHA-2(1,2) = "O" AND
                  LINHA-2(1,3) = "O"
                       PERFORM WIN
               END-IF.

               IF LINHA-2(2,1) = "O" AND LINHA-2(2,2) = "O" AND
                  LINHA-2(2,3) = "O"
                       PERFORM WIN
               END-IF.

               IF LINHA-2(3,1) = "O" AND LINHA-2(3,2) = "O" AND
                  LINHA-2(3,3) = "O"
                       PERFORM WIN
               END-IF.

               IF LINHA-2(1,1) = "O" AND LINHA-2(2,1) = "O" AND
                  LINHA-2(3,1) = "O"
                       PERFORM WIN
               END-IF.

               IF LINHA-2(1,2) = "O" AND LINHA-2(2,2) = "O" AND
                  LINHA-2(3,2) = "O"
                       PERFORM WIN
               END-IF.

               IF LINHA-2(1,3) = "O" AND LINHA-2(2,3) = "O" AND
                  LINHA-2(3,3) = "O"
                       PERFORM WIN
               END-IF.

               IF LINHA-2(1,3) = "O" AND LINHA-2(2,2) = "O" AND
                  LINHA-2(3,1) = "O"
                       PERFORM WIN
               END-IF.

               IF LINHA-2(1,1) = "O" AND LINHA-2(2,2) = "O" AND
                  LINHA-2(3,3) = "O"
                       PERFORM WIN
               END-IF.


           WIN.

               PERFORM EXIBIRTABULEIRO.
               DISPLAY "O JOGADOR " JOGADOR " VENCEU O JOGO! PARABENS!".
               STOP RUN.


           NOVAJOGADA.
               IF CONTADOR < 9
                   PERFORM SEQUENCIA
               ELSE
                   DISPLAY "O JOGO FICOU EMPATADO...BORING..."
               END-IF.

            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
