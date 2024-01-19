       IDENTIFICATION DIVISION.
       PROGRAM-ID. SVP003.
      *JORGE AUGUSTO DE OLIVEIRA ROCHA.
      ************************
      * CADASTRO DE FORNECEDOR   *
      ************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQFOR ASSIGN TO DISK
                       ORGANIZATION IS INDEXED
                       ACCESS MODE  IS DYNAMIC
                       RECORD KEY   IS FOR-TP 
                       FILE STATUS  IS ST-ERRO
                       ALTERNATE RECORD KEY IS FOR-CPF
                       WITH DUPLICATES.

           
           SELECT ARQCEP ASSIGN TO DISK
                        ORGANIZATION IS INDEXED
                        ACCESS MODE  IS DYNAMIC
                        RECORD KEY   IS CEP-NUMCEP
                        FILE STATUS  IS ST-ERRO
                        ALTERNATE RECORD KEY IS CEP-LOGRA
                        WITH DUPLICATES.
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD  ARQFOR
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQ.FORDAT".
       01 REGFOR.
           03 FOR-TP               PIC X(01).
           03 FOR-CPF              PIC 9(14).
           03 FOR-NOME             PIC X(30).
           03 FOR-FANTASIA         PIC X(12).
           03 FOR-DDD              PIC 9(02).
           03 FOR-NUMTEL           PIC 9(09).
           03 FOR-EMAIL            PIC X(30).
           03 FOR-CEP              PIC 9(08).
           03 FOR-CONTATO          PIC X(30).
           03 FOR-LONGRADOURO      PIC 9(05).
           03 FOR-COMPLEMENTO      PIC X(12).


       FD  ARQCEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQCEP.DAT".
       01 REGCEP.
           03 CEP-NUMCEP         PIC 9(08).
           03 CEP-LOGRA          PIC X(30).
           03 CEP-BAIRRO         PIC X(20).
           03 CEP-CIDADE         PIC X(20).
           03 CEP-UF             PIC X(02).	

      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
           01 W-OPCAO              PIC X(01) VALUE SPACES.
           01 W-ACT                PIC 9(02) VALUE ZEROS.
           01 ST-ERRO              PIC X(02) VALUE "00".
           01 W-CONT               PIC 9(06) VALUE ZEROS.
           01 MENS                 PIC X(50) VALUE SPACES.
           01 LIMPA                PIC X(50) VALUE SPACES.
           01 W-SEL                PIC 9(02) VALUE ZEROS.
           01 TXTTFOR             PIC X(10) VALUE SPACES.
       
      
       SCREEN SECTION.
       01 TELAX03.
           05 BLANK SCREEN.
           05 LINE 02 COLUMN 18 VALUE "* CADASTRO DE FORNECEDOR *".
           05 LINE 03 COLUMN 01 VALUE "TIPO PESSOA           :".
           05 LINE 05 COLUMN 01 VALUE "CPF/CNPJ              :".
           05 LINE 07 COLUMN 01 VALUE "NOME OU RAZAO SOCIAL   :".
           05 LINE 09 COLUMN 01 VALUE "NOME FANTASIA         :".
           05 LINE 11 COLUMN 01 VALUE "DDD TELEFONE          :".
           05 LINE 13 COLUMN 01 VALUE "NUM TELEFONE          :".
           05 LINE 15 COLUMN 01 VALUE "EMAIL                 :".
           05 LINE 17 COLUMN 01 VALUE "CEP                   :".
           05 LINE 19 COLUMN 01 VALUE "CONTATO               :".
           05 LINE 20 COLUMN 01 VALUE "LONGRADOURO           :".
           05 LINE 21 COLUMN 01 VALUE "COMPLEMENTO           :".
           05 LINE 23 COLUMN 01 VALUE "MENSAGEM              :".

          
           05 TFOR-TP
           LINE 03 COLUMN 26 PIC X(01)
           USING FOR-TP
           HIGHLIGHT.

           05 TFOR-CPF
               LINE 05 COLUMN 26 PIC 9(14)
               USING FOR-CPF
               HIGHLIGHT.

           05 TFOR-NOME
               LINE 07 COLUMN 26 PIC X(30)
               USING FOR-NOME
               HIGHLIGHT.

           05 TFOR-FANTASIA
               LINE 09 COLUMN 26 PIC X(12)
               USING FOR-FANTASIA
               HIGHLIGHT.

           05 TFOR-DDD
               LINE 11 COLUMN 26 PIC 9(02)
               USING FOR-DDD
               HIGHLIGHT.

           05 TFOR-NUMTEL
               LINE 13 COLUMN 26 PIC 9(09)
               USING FOR-NUMTEL
               HIGHLIGHT.

           05 TFOR-EMAIL
               LINE 15 COLUMN 26 PIC X(30)
               USING FOR-EMAIL
               HIGHLIGHT.

           05 TFOR-CEP
               LINE 17 COLUMN 26 PIC 9(08)
               USING FOR-CEP
               HIGHLIGHT.

           05 TFOR-CONTATO
               LINE 19 COLUMN 26 PIC X(30)
               USING FOR-CONTATO
               HIGHLIGHT.

           05 TFOR-LONGRADOURO
               LINE 20 COLUMN 26 PIC 9(05)
               USING FOR-LONGRADOURO
               HIGHLIGHT.

           05 TFOR-COMPLEMENTO
               LINE 21 COLUMN 26 PIC X(12)
               USING FOR-COMPLEMENTO
               HIGHLIGHT.

           05 TMENS
               LINE 23 COLUMN 26 PIC X(50)
               USING MENS
               HIGHLIGHT.
      *                    
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.

      *-------------ABERTURA DO ARQUIVO -------------------
       R0.
           OPEN I-O ARQFOR 
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT ARQFOR
                 CLOSE ARQFOR
                 MOVE "*** ARQUIVO ARQFOR FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO ARQFOR" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
       R0A.
           OPEN INPUT ARQCEP
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "*** ARQUIVO DE CEP NAO ENCONTRADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO DE CEP " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      *------------- INICIALIZACAO DAS VARIAVEIS------------
       R1.
           MOVE SPACES TO FOR-TP FOR-NOME FOR-FANTASIA FOR-EMAIL
           MOVE SPACES TO FOR-CONTATO FOR-COMPLEMENTO CEP-LOGRA
           MOVE SPACES TO CEP-BAIRRO CEP-CIDADE CEP-UF
           MOVE ZEROS TO FOR-CPF FOR-DDD FOR-NUMTEL FOR-CEP
           MOVE ZEROS TO FOR-LONGRADOURO W-SEL CEP-NUMCEP.

      *-------------VISUALIZACAO DA TELA ------------------
           DISPLAY TELAX03.
       R2. 
           ACCEPT TFOR-TP
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.
       LER-ARQFOR.
           READ ARQFOR
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
                DISPLAY TELAX03
                MOVE "*** FORNECEDOR JA CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO ARQFOR" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                MOVE "*** FORNECEDOR NAO CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM.
       R3.       
           ACCEPT TFOR-CPF
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2. 

       R4.       
           ACCEPT TFOR-NOME
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3. 
       R5.
           ACCEPT TFOR-FANTASIA.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R4.
       R6.
           ACCEPT TFOR-DDD.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R5.
       R7.
           ACCEPT TFOR-CONTATO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R6.
       R8.
           ACCEPT TFOR-CEP
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   DISPLAY TELAX03
                   GO TO R7.
           IF FOR-CEP = 0 
                 MOVE "*** CEP NAO INFORMADO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R8.
       R8A.
           MOVE FOR-CEP TO CEP-NUMCEP.
           READ ARQCEP
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "23"
                   MOVE "*** CEP DIGITADO NAO ENCONTRADO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R8
               ELSE
                  MOVE "ERRO NA LEITURA ARQUIVO CADASTRO CEP" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM
           ELSE
                DISPLAY TELAX03.

       R9.
           ACCEPT TFOR-LONGRADOURO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   DISPLAY TELAX03
                   GO TO R8.
           IF CEP-LOGRA = 0 
                 MOVE "** NUMERO RESIDENCIA NAO INFORMADO  **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R9.
       

       R10.
           ACCEPT TFOR-COMPLEMENTO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   DISPLAY TELAX03
                   GO TO R9.


      * ------------- VERICAR SE E ALTERACAO -----------------
           IF W-SEL = 1 
                GO TO ALT-OPC.
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R8.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGFOR
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1.
                IF ST-ERRO = "22"
                  MOVE "* FORNECEDOR JA EXISTE,DADOS NAO GRAVADOS *"
                  TO MENS 
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO R1
                ELSE
                      MOVE "ERRO NA GRAVACAO"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.

     
     *
      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
       ACE-001.
                DISPLAY (23, 12)
                     "N=NOVO REGISTRO   A=ALTERAR   E=EXCLUIR"
                ACCEPT (23, 55) W-OPCAO
                IF W-OPCAO NOT = "N" OR "n" AND W-OPCAO NOT = "A" OR "a" 
                    AND W-OPCAO NOT = "E" OR "e" GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (23, 12) MENS
                IF W-OPCAO = "N" OR "n"
                   GO TO R1  
                ELSE
                   IF W-OPCAO = "A" OR "a"
                      MOVE 1 TO W-SEL
                      GO TO R3.
      *
       EXC-OPC.
                DISPLAY (23, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE  ARQFOR RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO FORNECEDOR EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (23, 40) "ALTERAR  (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R7.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGFOR
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO EXCLUSAO DO REGISTRO FORNECEDOR"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *-------------------------------------------------------------------------------------------
       ROT-FIM.
           CLOSE ARQFOR ARQCEP.
           STOP RUN.
      *--------------------------------------------------------------
      *---------[ ROTINA DE MENSAGEM ]---------------------
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (23, 12) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   MOVE SPACES TO MENS
                   DISPLAY (23, 12) MENS.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.

      *    FILE STATUS
      *    00 = OPERACAO REALIZADO COM SUCESSO
      *    22 = REGISTRO JA CADASTRADO
      *    23 = REGISTRO NAO ENCONTRADO
      *    30 = ARQUIVO NAO ENCONTRADO