      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NotasIndexado.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "SLNOTAS.CBL".
       DATA DIVISION.
       FILE SECTION.
           COPY "FDNOTAS.CBL".
       WORKING-STORAGE SECTION.


       77 SAI                  PIC X VALUE 'N'.
       77 MEDIA                PIC 99V99.
       77 WS-NOTAS-NUMBER      PIC 9(5).
       77 WS-NOMEALUNO         PIC A(20).
       77 WS-NOTA1             PIC 99V99.
       77 WS-NOTA2             PIC 99V99.
       77 WS-NOTA3             PIC 99V99.
       77 WS-MEDIAALUNO        PIC 99V99.
       77 OPCAO                PIC 9.
       77 CONT                 PIC X VALUE "N".

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           MENU.
           INITIALIZE SAI
           INITIALIZE CONT
           DISPLAY "***      MENU      ***".
           DISPLAY "*** 1-INSERIR ALUNO ***".
           DISPLAY "*** 2-MOSTRAR LISTA ***".
           DISPLAY "*** 3-EDITAR ALUNO  ***".
           DISPLAY "*** 4-APAGAR ALUNO  ***".
           DISPLAY "***     0-SAIR      ***".

           ACCEPT OPCAO
           IF OPCAO = 1
               PERFORM INSERIR
           ELSE IF OPCAO = 2
               PERFORM MOSTRAR
           ELSE IF OPCAO = 3
               PERFORM EDITAR
           ELSE IF OPCAO = 4
               PERFORM APAGAR
           ELSE IF OPCAO = 0
               PERFORM FIM
           ELSE
               DISPLAY "OPCAO INVALIDA, DIGITE UM NR DE 0 A 4"
               PERFORM MENU
           .



           INSERIR.
           OPEN I-O NOTAS-FILE
           MOVE SPACE TO REG-ALUNO

           DISPLAY "POR FAVOR INSIRA O NR DO ALUNO: "
           ACCEPT NOTAS-NUMBER


           READ NOTAS-FILE KEY IS NOTAS-NUMBER
               INVALID KEY
                   DISPLAY "POR FAVOR INSIRA O NOME DO ALUNO: "
                   ACCEPT NOMEALUNO
               NOT INVALID KEY
                   DISPLAY "ERRO! NR DE ALUNO JA EXISTE."
                   CLOSE NOTAS-FILE
                   PERFORM INSERIR
           END-READ

           DISPLAY "POR FAVOR INSIRA A PRIMEIRA NOTA: "
           PERFORM UNTIL NOTA1 > 0 AND NOTA1 < 21
           ACCEPT NOTA1
           IF NOTA1 < 1 OR NOTA1 > 20
               DISPLAY "NOTA INVALIDA, INSIRA NOVAMENTE!"
           END-IF
           END-PERFORM

           DISPLAY "POR FAVOR INSIRA A SEGUNDA NOTA: "
           ACCEPT NOTA2
           PERFORM UNTIL NOTA2 > 0 AND NOTA2 < 21
           ACCEPT NOTA2
           IF NOTA2 < 1 OR NOTA2 > 20
               DISPLAY "NOTA INVALIDA, INSIRA NOVAMENTE!"
           END-IF
           END-PERFORM

           DISPLAY "POR FAVOR INSIRA A TERCEIRA NOTA: "
           ACCEPT NOTA3
           PERFORM UNTIL NOTA3 > 0 AND NOTA3 < 21
           ACCEPT NOTA3
           IF NOTA3 < 1 OR NOTA3 > 20
               DISPLAY "NOTA INVALIDA, INSIRA NOVAMENTE!"
           END-IF
           END-PERFORM

           COMPUTE MEDIAALUNO = (NOTA1 + NOTA2 + NOTA3) / 3

           WRITE REG-ALUNO
           CLOSE NOTAS-FILE
           DISPLAY "QUER INSERIR MAIS ALGUM REGISTO DE ALUNO?"
           DISPLAY "S PARA CONTINUAR, OUTRA TECLA QUALQUER PARA SAIR"
           ACCEPT CONT
           IF CONT = "S"
               PERFORM INSERIR
           ELSE
               PERFORM MENU
           .


           MOSTRAR.
           OPEN INPUT NOTAS-FILE
           PERFORM UNTIL SAI = 'S'
           READ NOTAS-FILE
               AT END
                   MOVE 'S' TO SAI
               NOT AT END
                   DISPLAY "ID: " NOTAS-NUMBER
                   DISPLAY "NOME: " NOMEALUNO
                   DISPLAY "NOTA 1: " NOTA1
                   DISPLAY "NOTA 2: " NOTA2
                   DISPLAY "NOTA 3: " NOTA3
                   DISPLAY "MEDIA: " MEDIAALUNO
                   DISPLAY " "
           END-READ
           END-PERFORM
           CLOSE NOTAS-FILE
           PERFORM MENU
           .

           EDITAR.
           OPEN I-O NOTAS-FILE
               DISPLAY "DIGITE O NR DE ALUNO QUE PRETENDE EDITAR: "
               DISPLAY "0 PARA SAIR"
               ACCEPT WS-NOTAS-NUMBER
               IF WS-NOTAS-NUMBER = 0
                   CLOSE NOTAS-FILE
                 PERFORM MENU
               END-IF


           MOVE WS-NOTAS-NUMBER TO NOTAS-NUMBER

           READ NOTAS-FILE KEY IS NOTAS-NUMBER
               INVALID KEY
                   DISPLAY "ERRO! NR DE ALUNO NAO ENCONTRADO."
                   CLOSE NOTAS-FILE
                   PERFORM EDITAR
               NOT INVALID KEY
                   DISPLAY "REGISTO ATUAL: "
                   DISPLAY "NOME ALUNO: " NOMEALUNO
                   DISPLAY "NOTA 1: " NOTA1
                   DISPLAY "NOTA 2: " NOTA2
                   DISPLAY "NOTA 3: " NOTA3

           DISPLAY "DIGITE NOVO NOME DE ALUNO: (ENTER PARA MANTER)"
           ACCEPT WS-NOMEALUNO
           IF WS-NOMEALUNO NOT = SPACE
               MOVE WS-NOMEALUNO TO NOMEALUNO
           END-IF

           DISPLAY "DIGITE NOVA NOTA 1: (ENTER PARA MANTER)"
           ACCEPT WS-NOTA1
           IF WS-NOTA1 NOT = ZERO
               MOVE WS-NOTA1 TO NOTA1
           END-IF

           DISPLAY "DIGITE NOVA NOTA 2: (ENTER PARA MANTER)"
           ACCEPT WS-NOTA2
           IF WS-NOTA2 NOT = ZERO
               MOVE WS-NOTA2 TO NOTA2
           END-IF

           DISPLAY "DIGITE NOVA NOTA 3: (ENTER PARA MANTER)"
           ACCEPT WS-NOTA3
           IF WS-NOTA3 NOT = ZERO
               MOVE WS-NOTA3 TO NOTA3
           END-IF

           COMPUTE WS-MEDIAALUNO = (NOTA1 + NOTA2 + NOTA3) / 3
           MOVE WS-MEDIAALUNO TO MEDIAALUNO

           REWRITE REG-ALUNO
           END-READ
           CLOSE NOTAS-FILE

           DISPLAY "QUER EDITAR MAIS ALGUM REGISTO DE ALUNO?"
           DISPLAY "S PARA CONTINUAR, OUTRA TECLA QUALQUER PARA SAIR"
           ACCEPT CONT
           IF CONT = "S"
               PERFORM EDITAR
           ELSE
               PERFORM MENU
           .

           APAGAR.
           OPEN I-O NOTAS-FILE
           DISPLAY "DIGITE O NR DE ALUNO QUE PRETENDE APAGAR: "
               DISPLAY "0 PARA SAIR"
               ACCEPT WS-NOTAS-NUMBER
               IF WS-NOTAS-NUMBER = 0
                   CLOSE NOTAS-FILE
                  PERFORM MENU
               END-IF

           MOVE WS-NOTAS-NUMBER TO NOTAS-NUMBER

           READ NOTAS-FILE KEY IS NOTAS-NUMBER
               INVALID KEY
                   DISPLAY "ERRO! NR DE ALUNO NAO ENCONTRADO."
                   CLOSE NOTAS-FILE
                   PERFORM APAGAR
               NOT INVALID KEY
                   DELETE NOTAS-FILE
                   DISPLAY "REGISTO APAGADO COM SUCESSO!"
                   DISPLAY " "
           END-READ
           CLOSE NOTAS-FILE
           DISPLAY "QUER APAGAR MAIS ALGUM REGISTO DE ALUNO?"
           DISPLAY "S PARA CONTINUAR, OUTRA TECLA QUALQUER PARA SAIR"
           ACCEPT CONT
           IF CONT = "S"
               PERFORM APAGAR
           ELSE
               PERFORM MENU
           .
           FIM.
           STOP RUN.
       END PROGRAM NotasIndexado.
