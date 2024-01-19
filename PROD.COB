       IDENTIFICATION DIVISION.
       PROGRAM-ID. SVP004.
      *JORGE AUGUSTO DE OLIVEIRA ROCHA.
      ************************
      * CADASTRO DE PRODUTO   *
      ************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQPROD ASSIGN TO DISK
                       ORGANIZATION IS INDEXED
                       ACCESS MODE  IS DYNAMIC
                       RECORD KEY   IS PROD-COD 
                       FILE STATUS  IS ST-ERRO
                       ALTERNATE RECORD KEY IS PROD-DESC
                       WITH DUPLICATES.

           
           SELECT ARQFOR ASSIGN TO DISK
                       ORGANIZATION IS INDEXED
                       ACCESS MODE  IS DYNAMIC
                       RECORD KEY   IS FOR-TP 
                       FILE STATUS  IS ST-ERRO
                       ALTERNATE RECORD KEY IS FOR-CPF
                       WITH DUPLICATES.
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD  ARQPROD
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQPROD.DAT".
       01 REGPROD.
           03 PROD-COD               PIC 9(06).
           03 PROD-DESC              PIC X(30).
           03 PROD-UNID              PIC X(02).
           03 PROD-TPPROD            PIC 9(01).
           03 PROD-FORNEC            PIC 9(14).
           03 PROD-ULTIMA            PIC 9(08).
           03 PROD-PRECOUN           PIC 9(05).
           03 PROD-ESTOQ             PIC 9(06).
        
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

      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
           01 W-OPCAO              PIC X(01) VALUE SPACES.
           01 W-ACT                PIC 9(02) VALUE ZEROS.
           01 ST-ERRO              PIC X(02) VALUE "00".
           01 W-CONT               PIC 9(06) VALUE ZEROS.
           01 MENS                 PIC X(50) VALUE SPACES.
           01 LIMPA                PIC X(50) VALUE SPACES.
           01 W-SEL                PIC 9(02) VALUE ZEROS.
           01 TXTSERPROD             PIC X(10) VALUE SPACES.
           01 TXTUNPROD              PIC X(12) VALUE SPACES.

       
      
       SCREEN SECTION.
       01 TELAX04.
           05 BLANK SCREEN.
           05 LINE 02 COLUMN 18 VALUE "* CADASTRO DE PRODUTO *".
           05 LINE 03 COLUMN 01 VALUE "COD PRODUTO           :".
           05 LINE 05 COLUMN 01 VALUE "DESCRICAO PRODUTO     :".
           05 LINE 07 COLUMN 01 VALUE "UNIDADE PRODUTO       :".
           05 LINE 09 COLUMN 01 VALUE "TIPO PRODUTO          :".
           05 LINE 11 COLUMN 01 VALUE "ULTIMA COMPRA         :".
           05 LINE 13 COLUMN 01 VALUE "PRECO UNIDADE         :".
           05 LINE 15 COLUMN 01 VALUE "ESTOQUE PRODUTO       :".          
           05 LINE 17 COLUMN 01 VALUE "FORNECEDOR            :".
           05 LINE 19 COLUMN 01 VALUE "CONTATO FORNECEDOR    :".
           05 LINE 23 COLUMN 01 VALUE "MENSAGEM              :".
     
          
           05 TPROD-COD
               LINE 03 COLUMN 26 PIC 9(06).
               USING PROD-COD
               HIGHLIGHT.

           05 TPROD-DESC
               LINE 05 COLUMN 26 PIC X(30)
               USING PROD-DESC
               HIGHLIGHT.
           
           05 TPROD-UNID
               LINE 07 COLUMN 26  PIC X(01).
               USING PROD-UNID
               HIGHLIGHT.
           05 TTXTUNVPROD
               LINE 7  COLUMN 28  PIC X(6)
               USING  TXTUNPROD
               HIGHLIGHT.
           
           05 TPROD-TP
               LINE 9 COLUMN 26  PIC X(01).
               USING PROD-TPPROD
               HIGHLIGHT.
           05 TTXTSERVPROD
               LINE 9  COLUMN 28  PIC X(12)
               USING  TXTSERVPROD
               HIGHLIGHT.

           05 TPROD-ULTIMA
               LINE 11 COLUMN 26 PIC XX/XX/XXXX
               USING PROD-ULTIMA
               HIGHLIGHT.

           05 TPROD-PRECOUN
               LINE 13 COLUMN 26  PIC 09V02.
               USING PROD-PRECOUN
               HIGHLIGHT.
           
           05 TPROD-ESTOQ
               LINE 15 COLUMN 26  PIC 9(06).
               USING PROD-ESTOQ
               HIGHLIGHT.
           
           05  TFOR-CPF
               LINE 17  COLUMN 26  PIC 9(14)
               USING  FOR-CPF
               HIGHLIGHT.

           05  TFOR-CONTATO
               LINE 19  COLUMN 26  PIC X(30)
               USING  FOR-CONTATO
               HIGHLIGHT.

           05 TMENS
               LINE 23 COLUMN 26 PIC X(50)
               USING MENS
               HIGHLIGHT.

           01  TELAX04UNID.
           05  LINE 11  COLUMN 45 
               VALUE  "        01-LITRO".
           05  LINE 12  COLUMN 45 
               VALUE  "        02-KILO".
           05  LINE 13  COLUMN 45 
               VALUE  "        03-TONELADA".
           05  LINE 14  COLUMN 45 
               VALUE  "        04-PECA".
           05  LINE 15  COLUMN 45 
               VALUE  "        05-UNIDADE".

       01  TELAX04SERV.
           05  LINE 11  COLUMN 45 
               VALUE  "        01-SAUDE".
           05  LINE 12  COLUMN 45 
               VALUE  "        02-ALIMENTO".
           05  LINE 13  COLUMN 45 
               VALUE  "        03-VESTUARIO".
           05  LINE 14  COLUMN 45 
               VALUE  "        04-CALCADO".
           05  LINE 15  COLUMN 45 
               VALUE  "        05-BEBIDA".
           05  LINE 16  COLUMN 45 
               VALUE  "        06-SEGURANCA".
           05  LINE 17  COLUMN 45 
               VALUE  "        07-LAZER".
           05  LINE 18  COLUMN 45 
               VALUE  "        08-HOSPEDAGEM".
           05  LINE 19  COLUMN 45 
               VALUE  "        09-BELEZA".

      *                    
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.

      *-------------ABERTURA DO ARQUIVO -------------------
       R0.
           OPEN I-O ARQPROD 
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT ARQPROD
                 CLOSE ARQPROD
                 MOVE "*** ARQUIVO ARQPORD FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO ARQPROD" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
       R0A.
           OPEN INPUT ARQFOR
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "*** ARQUIVO FORNECEDOR NAO ENCONTRADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO FORNECEDOR " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      *------------- INICIALIZACAO DAS VARIAVEIS------------
       R1.
           
           MOVE SPACES TO PROD-DESC  TXTSERPROD TXTUNPROD 
              FOR-TP  FOR-NOME FOR-FANTASIA PROD-UNID
               FOR-EMAIL FOR-CONTATO FOR-COMPLEMENTO.

           MOVE ZEROS TO PROD-COD W-SEL PROD-ESTOQ  PROD-TPPROD 
              PROD-FORNEC PROD-ULTIMA PROD-PRECOUN PROD-ESTOQ FOR-CPF
              FOR-DDD FOR-NUMTEL   FOR-CEP FOR-LONGRADOURO.

      *-------------VISUALIZACAO DA TELA ------------------
           DISPLAY TELAX04.
       R2. 
           ACCEPT TPROD-COD
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.
       LER-ARQPROD.
           READ ARQPROD
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
                DISPLAY TELAX04
                MOVE "*** PRODUTO JA CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO ARQPROD" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                MOVE "*** PRODUTO NAO CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM.
       R3.       
           ACCEPT TPROD-DESC
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2. 

       R4. 
           DISPLAY TELAX04UNID. 

           ACCEPT TPROD-UNID
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3. 
       R4A.
           IF PROD-UNID = "1"
              MOVE "LITRO" TO TXTUNPROD
           ELSE
           
           IF PROD-UNID = "2"
              MOVE "KILO" TO TXTUNPROD
           ELSE
                               
           IF PROD-UNID = "3"
              MOVE "TONELADA" TO TXTUNPROD
           ELSE 
           IF PROD-UNID = "4"
              MOVE "PECA" TO TXTUNPROD
           ELSE

           IF PROD-UNID = "5"
              MOVE "UNIDADE" TO TXTUNPROD
           ELSE

           IF PROD-UNID = "6"
              MOVE "DIAS" TO TXTUNPROD
             
           ELSE
                 MOVE "*DIGITE UM NUMERO*" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R4.
           DISPLAY TTXTUNVPROD.
           DISPLAY TELAX04.
            
       R5.
           DISPLAY TELAX04SERV
           ACCEPT TXTSERVPROD.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R4.
       . 
       R5A.
           IF PROD-TPPROD = "1"
              MOVE "SAUDE" TO TXTSERPROD
           ELSE
           
           IF PROD-TPPROD = "2"
              MOVE "ALIMENTO" TO TXTSERPROD
           ELSE
                               
           IF PROD-TPPROD = "3"
              MOVE "VESTUARIO" TO TXTSERPROD
           ELSE 
           IF PROD-TPPROD = "4"
              MOVE "CALCADO" TO TXTSERPROD
           ELSE

           IF PROD-TPPROD = "5"
              MOVE "BEBIDA" TO TXTSERPROD
           ELSE

           IF PROD-TPPROD = "6"
              MOVE "SEGURANCA" TO TXTSERPROD
           ELSE

           IF PROD-TPPROD = "7"
              MOVE "LAZER" TO TXTSERPROD
           ELSE

           IF PROD-TPPROD = "8"
              MOVE "HOSPEDAGEM" TO TXTSERPROD
           ELSE

           IF PROD-TPPROD = "9"
              MOVE "BELEZA" TO TXTSERPROD

           ELSE
                 MOVE "*DIGITE UM NUMERO*" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R5.
           DISPLAY TTXTSERVPROD.
           DISPLAY TELAX04.

       R6.
           ACCEPT TPROD-ULTIMA .
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R5.
       R7.
           ACCEPT TPROD-PRECOUN.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R6.
       R8.
           ACCEPT TPROD-ESTOQ.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R7.

       R9.
           ACCEPT TFOR-CPF
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R8.
           IF CEP-LOGRA = 0 
                 MOVE "** FORNECEDOR NAO INFORMADO  **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R9.
       R9A.
           MOVE FOR-CPF TO PROD-FORNEC.
           READ ARQFOR
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "23"
                   MOVE "*** FORNECEDOR NAO ENCONTRADO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R8
               ELSE
                  MOVE "ERRO NA LEITURA ARQUIVO CADASTRO FORNEC" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM
           ELSE
                DISPLAY TELAX04.
       
       R10.
           ACCEPT TFOR-CONTATO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   DISPLAY TELAX04
                   GO TO R8.
           IF FOR-CONTATO = 0 
                 MOVE "** NUMERO RESIDENCIA NAO INFORMADO  **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
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
                WRITE REGPROD
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1.
                IF ST-ERRO = "22"
                  MOVE "* PRODUTO JA EXISTE,DADOS NAO GRAVADOS *"
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
                DELETE  ARQPROD RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO EXCLUIDO ***" TO MENS
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
                REWRITE REGPROD
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO EXCLUSAO DO REGISTRO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *-------------------------------------------------------------------------------------------
       ROT-FIM.
           CLOSE ARQPROD ARQFOR.
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