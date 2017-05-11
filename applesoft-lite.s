; Applesoft Lite
;
; Disassembled from the Apple II+ ROMs with da65 V2.12.0
;
; Most comments and label names from the S-C DocuMentor
; by Bob Sander-Cederlof
;
; Adapted for the Replica-1 by Tom Greene
; 7-May-2008
;
; v0.4



.setcpu "6502"
.segment "BASIC"

.export FIX_LINKS, ERROR, INPUTBUFFER
.exportzp ERR_SYNTAX, ERR_NOCFFA

.import CLS, OUTDO, CRDO, OUTSP, OUTQUES	; Imports from io.s
.import KEYBOARD, GETLN, RDKEY

.import CFFALoad, CFFASave, CFFAMenu		; Imports from cffa1.s

.include "macros.s"
.include "zeropage.s"


; ----------------------------------------------------------------------------
STACK		:= $0100
INPUTBUFFER	:= $0200

; ----------------------------------------------------------------------------
; Applesoft Tokens
; ----------------------------------------------------------------------------
TOKEN_FOR	= $81
TOKEN_DATA	= $83
TOKEN_POP	= $88
TOKEN_GOTO	= $8E
TOKEN_GOSUB	= $92
TOKEN_REM	= $94
TOKEN_PRINT	= $98
TOKEN_TO	= $A2
TOKEN_SPC	= $A3
TOKEN_THEN	= $A4
TOKEN_NOT	= $A5
TOKEN_STEP	= $A6
TOKEN_PLUS	= $A7
TOKEN_MINUS	= $A8
TOKEN_GREATER	= $AE
TOKEN_EQUAL	= $AF
TOKEN_SGN	= $B1
TOKEN_LEFTSTR	= $BF


; ----------------------------------------------------------------------------
; Cold and warm entry points at $E000 and E003
; ----------------------------------------------------------------------------
	jmp	COLDSTART
	jmp	RESTART


; ----------------------------------------------------------------------------
; Branch Table for Tokens
; ----------------------------------------------------------------------------
TOKEN_ADDRESS_TABLE:
	.addr	END - 1		; $80... 128... END
	.addr	FOR - 1		; $81... 129... FOR
	.addr	NEXT - 1	; $82... 130... NEXT
	.addr	DATA - 1	; $83... 131... DATA
	.addr	INPUT - 1	; $84... 132... INPUT
	.addr	DIM - 1		; $85... 133... DIM
	.addr	READ - 1	; $86... 134... READ
	.addr	CALL - 1	; $87... 135... CALL
	.addr	POP - 1		; $88... 136... POP
	.addr	HIMEM - 1	; $89... 136... HIMEM:
	.addr	LOMEM - 1	; $8A... 137... LOMEM:
	.addr	ONERR - 1	; $8B... 138... ONERR
	.addr	RESUME - 1	; $8C... 139... RESUME
	.addr	LET - 1		; $8D... 140... LET
	.addr	GOTO - 1	; $8E... 141... GOTO
	.addr	RUN - 1		; $8F... 142... RUN
	.addr	IF - 1		; $90... 143... IF
	.addr	RESTORE - 1	; $91... 144... RESTORE
	.addr	GOSUB - 1	; $92... 145... GOSUB
	.addr	POP - 1		; $93... 146... RETURN
	.addr	REM - 1		; $94... 147... REM
	.addr	STOP - 1	; $95... 148... STOP
	.addr	ONGOTO - 1	; $96... 149... ON
	.addr	POKE - 1	; $97... 150... POKE
	.addr	PRINT - 1	; $98... 151... PRINT
	.addr	CONT - 1	; $99... 152... CONT
	.addr	LIST - 1	; $9A... 153... LIST
	.addr	CLEAR - 1	; $9B... 154... CLEAR
	.addr	GET - 1		; $9C... 155... GET
	.addr	NEW - 1		; $9D... 156... NEW
	.addr	CFFAMenu - 1	; $9E... 157... MENU
	.addr	CFFASave - 1	; $9F... 158... SAVE
	.addr	CFFALoad - 1	; $A0... 160... LOAD
	.addr	CLS - 1		; $A1... 161... CLS
; ----------------------------------------------------------------------------
UNFNC:  .addr	SGN		; $B1... 177... SGN
	.addr	INT		; $B2... 178... INT
	.addr	ABS		; $B3... 179... ABS
	.addr	FRE		; $B4... 180... FRE
	.addr	SQR		; $B5... 181... SQR
	.addr	RND		; $B6... 182... RND
	.addr	LOG		; $B7... 183... LOG
	.addr	EXP		; $B8... 184... EXP
	.addr	PEEK		; $B9... 185... PEEK
	.addr	LEN		; $BA... 186... LEN
	.addr	STR		; $BB... 187... STR$
	.addr	VAL		; $BC... 188... VAL
	.addr	ASC		; $BD... 189... ASC
	.addr	CHRSTR		; $BE... 190... CHR$
	.addr	LEFTSTR		; $BF... 191... LEFT$
	.addr	RIGHTSTR	; $C0... 192... RIGHT$
	.addr	MIDSTR		; $C1... 193... MID$


; ----------------------------------------------------------------------------
; Math Operator Branch Table
;
; one-byte precedence code
; two-byte address
; ----------------------------------------------------------------------------
POR		= $46	; "OR" is lowest precedence
PAND		= $50
PREL		= $64	; Relational operators
PADD		= $79	; binary + and -
PMUL		= $7B	; * and /
PPWR		= $7D	; exponentiation
PNEQ		= $7F	; unary - and comparison =
; ----------------------------------------------------------------------------
MATHTBL:.byte   PADD
	.addr	FADDT - 1	; $A7... 167... +
	.byte	PADD
	.addr	FSUBT - 1	; $A8... 168... -
	.byte	PMUL
	.addr	FMULTT - 1	; $A9... 169... *
	.byte	PMUL
	.addr	FDIVT - 1	; $AA... 170... /
	.byte	PPWR
	.addr	FPWRT - 1	; $AB... 171... ^
	.byte	PAND
	.addr	TAND - 1	; $AC... 172... AND
	.byte	POR
	.addr	OR - 1		; $AD... 173... OR
M_NEG:	.byte	PNEQ
	.addr	NEGOP - 1	; $AE... 174... >
M_EQU:	.byte	PNEQ
	.addr	EQUOP - 1	; $AF... 175... =
M_REL:	.byte	PREL
	.addr	RELOPS - 1	; $B0... 176... <


; ----------------------------------------------------------------------------
; Token Name Table
; ----------------------------------------------------------------------------
TOKEN_NAME_TABLE:
	htasc	"END"		; $80... 128
	htasc	"FOR"		; $81... 129
	htasc	"NEXT"		; $82... 130
	htasc	"DATA"		; $83... 131
	htasc	"INPUT"		; $84... 132
	htasc	"DIM"		; $85... 133
	htasc	"READ"		; $86... 134
	htasc	"CALL"		; $87... 135
	htasc	"POP"		; $88... 136
	htasc	"HIMEM:"	; $89... 137
	htasc	"LOMEM:"	; $8A... 138
	htasc	"ONERR"		; $8B... 139
	htasc	"RESUME"	; $8C... 140
	htasc	"LET"		; $8D... 141
	htasc	"GOTO"		; $8E... 142
	htasc	"RUN"		; $8F... 143
	htasc	"IF"		; $90... 144
	htasc	"RESTORE"	; $91... 145
	htasc	"GOSUB"		; $92... 146
	htasc	"RETURN"	; $93... 147
	htasc	"REM"		; $94... 148
	htasc	"STOP"		; $95... 149
	htasc	"ON"		; $96... 150
	htasc	"POKE"		; $97... 151
	htasc	"PRINT"		; $98... 152
	htasc	"CONT"		; $99... 153
	htasc	"LIST"		; $9A... 154
	htasc	"CLEAR"		; $9B... 155
	htasc	"GET"		; $9C... 156
	htasc	"NEW"		; $9D... 157
	htasc	"MENU"		; $9E... 158   New tokens 
	htasc	"SAVE"		; $9F... 159   for
	htasc	"LOAD"		; $A0... 160   CFFA I/O
	htasc	"CLS"		; $A1... 161   New token to clear screen
	htasc	"TO"		; $A2... 162
	htasc	"SPC("		; $A3... 163
	htasc	"THEN"		; $A4... 164
	htasc	"NOT"		; $A5... 165
	htasc	"STEP"		; $A6... 166
	htasc	"+"		; $A7... 167
	htasc	"-"		; $A8... 168
	htasc	"*"		; $A9... 169
	htasc	"/"		; $AA... 170
	htasc	"^"		; $AB... 171
	htasc	"AND"		; $AC... 172
	htasc	"OR"		; $AD... 173
	htasc	">"		; $AE... 174
	htasc	"="		; $AF... 175
	htasc	"<"		; $B0... 176
	htasc	"SGN"		; $B1... 177
	htasc	"INT"		; $B2... 178
	htasc	"ABS"		; $B3... 179
	htasc	"FRE"		; $B4... 180
	htasc	"SQR"		; $B5... 181
	htasc	"RND"		; $B6... 182
	htasc	"LOG"		; $B7... 183
	htasc	"EXP"		; $B8... 184
	htasc	"PEEK"		; $B9... 185
	htasc	"LEN"		; $BA... 186
	htasc	"STR$"		; $BB... 187
	htasc	"VAL"		; $BC... 188
	htasc	"ASC"		; $BD... 189
	htasc	"CHR$"		; $BE... 190
	htasc	"LEFT$"		; $BF... 191
	htasc	"RIGHT$"	; $C0... 192
	htasc	"MID$"		; $C1... 193
	.byte   $00		; END OF TOKEN NAME TABLE


; ----------------------------------------------------------------------------
; Error Messages
; ----------------------------------------------------------------------------
ERROR_MESSAGES:

ERR_NOFOR	:= <(*-ERROR_MESSAGES)
	htasc	"NO FOR"

ERR_SYNTAX	:= <(*-ERROR_MESSAGES)
	htasc	"SYNTAX"

ERR_NOGOSUB	:= <(*-ERROR_MESSAGES)
	htasc	"NO GOSUB"

ERR_NODATA	:= <(*-ERROR_MESSAGES)
	htasc	"OUT OF DATA"

ERR_ILLQTY	:= <(*-ERROR_MESSAGES)
	htasc	"ILLEG QTY"

ERR_OVERFLOW	:= <(*-ERROR_MESSAGES)
	htasc	"OVERFLOW"

ERR_MEMFULL	:= <(*-ERROR_MESSAGES)
	htasc	"OUT OF MEM"

ERR_UNDEFSTAT	:= <(*-ERROR_MESSAGES)
	htasc	"UNDEF LINE"

ERR_BADSUBS	:= <(*-ERROR_MESSAGES)
	htasc	"BAD SUBSCR"

ERR_REDIMD	:= <(*-ERROR_MESSAGES)
	htasc	"REDIM"

ERR_ZERODIV	:= <(*-ERROR_MESSAGES)
	htasc	"DIV BY 0"

ERR_ILLDIR	:= <(*-ERROR_MESSAGES)
	htasc	"NOT DIRECT"

ERR_BADTYPE	:= <(*-ERROR_MESSAGES)
	htasc	"WRONG TYP"

ERR_STRLONG	:= <(*-ERROR_MESSAGES)
	htasc	"LONG STR"

ERR_FRMCPX	:= <(*-ERROR_MESSAGES)
	htasc	"LONG FORMULA"

ERR_CANTCONT	:= <(*-ERROR_MESSAGES)
	htasc	"CAN'T CONT"

ERR_NOCFFA	:= <(*-ERROR_MESSAGES)	; New error message for CFFA1 I/O
	htasc	"NO CFFA"
; ----------------------------------------------------------------------------
QT_ERROR:
	.byte	" ERR"
	.byte	$00
QT_IN:	.byte	" IN "
	.byte	$00
QT_BREAK:
	.byte	$0D
	.byte	"BREAK"
	.byte	$00

; ----------------------------------------------------------------------------
; CALLED BY "NEXT" AND "FOR" TO SCAN THROUGH
; THE STACK FOR A FRAME WITH THE SAME VARIABLE.
;
; (FORPNT) = ADDRESS OF VARIABLE IF "FOR" OR "NEXT"
; 	= $XXFF IF CALLED FROM "RETURN"
; 	<<< BUG: SHOULD BE $FFXX >>>
;
;	RETURNS .NE. IF VARIABLE NOT FOUND,
;	(X) = STACK PNTR AFTER SKIPPING ALL FRAMES
;
;	.EQ. IF FOUND
;	(X) = STACK PNTR OF FRAME FOUND
; ----------------------------------------------------------------------------
GTFORPNT:
	tsx
	inx
	inx
	inx
	inx
@1:	lda	STACK+1,x	; "FOR" FRAME HERE?
	cmp	#TOKEN_FOR
	bne	@4		; NO
	lda	FORPNT+1	; YES -- "NEXT" WITH NO VARIABLE?
	bne	@2		; NO, VARIABLE SPECIFIED
	lda	STACK+2,x	; YES, SO USE THIS FRAME
	sta	FORPNT
	lda	STACK+3,x
	sta	FORPNT+1
@2:	cmp	STACK+3,x	; IS VARIABLE IN THIS FRAME?
	bne	@3		; NO
	lda	FORPNT		; LOOK AT 2ND BYTE TOO
	cmp	STACK+2,x	; SAME VARIABLE?
	beq	@4		; YES
@3:	txa			; NO, SO TRY NEXT FRAME (IF ANY)
	clc			; 18 BYTES PER FRAME
	adc     #18
	tax
	bne	@1		; ...ALWAYS?
@4:	rts


; ----------------------------------------------------------------------------
; MOVE BLOCK OF MEMORY UP
;
; ON ENTRY:
;	(Y,A) = (HIGHDS) = DESTINATION END+1
;	(LOWTR) = LOWEST ADDRESS OF SOURCE
;	(HIGHTR) = HIGHEST SOURCE ADDRESS+1
; ----------------------------------------------------------------------------
BLTU:	jsr	REASON		; BE SURE (Y,A) < FRETOP
	sta	STREND		; NEW TOP OF ARRAY STORAGE
	sty	STREND+1
BLTU2:	sec
	lda	HIGHTR		; COMPUTE # OF BYTES TO BE MOVED
	sbc	LOWTR		; 	(FROM LOWTR THRU HIGHTR-1)
	sta	INDEX		; PARTIAL PAGE AMOUNT
	tay
	lda	HIGHTR+1
	sbc	LOWTR+1
	tax			; # OF WHOLE PAGES IN X-REG
	inx
	tya			; # BYTES IN PARTIAL PAGE
	beq	@4		; NO PARTIAL PAGE
	lda	HIGHTR		; BACK UP HIGHTR # BYTES IN PARTIAL PAGE
	sec
	sbc	INDEX
	sta	HIGHTR
	bcs	@1
	dec	HIGHTR+1
	sec
@1:	lda	HIGHDS		; BACK UP HIGHDS # BYTES IN PARTIAL PAGE
	sbc	INDEX
	sta	HIGHDS
	bcs	@3
	dec	HIGHDS+1
	bcc	@3		; ...ALWAYS
@2:	lda	(HIGHTR),y	; MOVE THE BYTES
	sta	(HIGHDS),y
@3:	dey
	bne	@2		; LOOP TO END OF THIS 256 BYTES
	lda	(HIGHTR),y	; MOVE ONE MORE BYTE
	sta	(HIGHDS),y
@4:	dec	HIGHTR+1	; DOWN TO NEXT BLOCK OF 256
	dec	HIGHDS+1
	dex			; ANOTHER BLOCK OF 256 TO MOVE?
	bne	@3		; YES
	rts			; NO, FINISHED


; ----------------------------------------------------------------------------
; CHECK IF ENOUGH ROOM LEFT ON STACK
; FOR "FOR", "GOSUB", OR EXPRESSION EVALUATION
; ----------------------------------------------------------------------------
CHKMEM:	asl	a
	adc	#54
	bcs	MEMERR		; ...MEM FULL ERR
	sta	INDEX
	tsx
	cpx	INDEX
	bcc	MEMERR		; ...MEM FULL ERR
	rts


; ----------------------------------------------------------------------------
; CHECK IF ENOUGH ROOM BETWEEN ARRAYS AND STRINGS
; (Y,A) = ADDR ARRAYS NEED TO GROW TO
; ----------------------------------------------------------------------------
REASON:	cpy	FRETOP+1	; HIGH BYTE
	bcc	@4		; PLENTY OF ROOM
	bne	@1		; NOT ENOUGH, TRY GARBAGE COLLECTION
	cmp	FRETOP		; LOW BYTE
	bcc	@4		; ENOUGH ROOM
@1:	pha			; SAVE (Y,A), TEMP1, AND TEMP2
	ldx	#FAC-TEMP1-1
	tya
@2:	pha
	lda	TEMP1,x
	dex
	bpl	@2
	jsr	GARBAG		; MAKE AS MUCH ROOM AS POSSIBLE
	ldx	#TEMP1-FAC+1	; RESTORE TEMP1 AND TEMP2
@3:	pla			; AND (Y,A)
	sta	FAC,x
	inx
	bmi	@3
	pla
	tay
	pla			; DID WE FIND ENOUGH ROOM?
	cpy	FRETOP+1	; HIGH BYTE
	bcc	@4		; YES, AT LEAST A PAGE
	bne	MEMERR		; NO, MEM FULL ERR
	cmp	FRETOP		; LOW BYTE
	bcs	MEMERR		; NO, MEM FULL ERR
@4:	rts			; YES, RETURN

; ----------------------------------------------------------------------------
MEMERR:	ldx	#ERR_MEMFULL


; ----------------------------------------------------------------------------
; HANDLE AN ERROR
;
; (X)=OFFSET IN ERROR MESSAGE TABLE
; (ERRFLG) > 128 IF "ON ERR" TURNED ON
; (CURLIN+1) = $FF IF IN DIRECT MODE
; ----------------------------------------------------------------------------
ERROR:	bit	ERRFLG		; "ON ERR" TURNED ON?
	bpl	@1		; NO
	jmp	HANDLERR	; YES
@1:	jsr	CRDO		; PRINT <RETURN>
	jsr	OUTQUES		; PRINT "?"
@2:	lda	ERROR_MESSAGES,x
	pha			; PRINT MESSAGE
	jsr	OUTDO
	inx
	pla
	bpl	@2
	jsr	STKINI		; FIX STACK, ET AL
	lda	#<QT_ERROR	; PRINT " ERROR" AND BELL
	ldy	#>QT_ERROR


; ----------------------------------------------------------------------------
; PRINT STRING AT (Y,A)
; PRINT CURRENT LINE # UNLESS IN DIRECT MODE
; FALL INTO WARM RESTART
; ----------------------------------------------------------------------------
PRINT_ERROR_LINNUM:
	jsr	STROUT		; PRINT STRING AT (Y,A)
	ldy	CURLIN+1	; RUNNING, OR DIRECT?
	iny
	beq	RESTART		; WAS $FF, SO DIRECT MODE
	jsr	INPRT		; RUNNING, SO PRINT LINE NUMBER


; ----------------------------------------------------------------------------
; WARM RESTART ENTRY
;
; COME HERE FROM MONITOR BY CTL-C, 0G, 3D0G, OR E003G
; ----------------------------------------------------------------------------
RESTART:
	jsr	CRDO		; PRINT <RETURN>
	ldx	#']' + $80	; PROMPT CHARACTER
	jsr	INLIN2		; READ A LINE
	stx	TXTPTR		; SET UP CHRGET TO SCAN THE LINE
	sty	TXTPTR+1
	lsr	ERRFLG		; CLEAR FLAG
	jsr	CHRGET
	tax
	beq	RESTART		; EMPTY LINE
	ldx	#$FF		; $FF IN HI-BYTE OF CURLIN MEANS
	stx	CURLIN+1	; WE ARE IN DIRECT MODE
	bcc	NUMBERED_LINE	; CHRGET SAW DIGIT, NUMBERED LINE
	jsr	PARSE_INPUT_LINE	; NO NUMBER, SO PARSE IT
	jmp	NEWSTT2		; AND TRY EXECUTING IT


; ----------------------------------------------------------------------------
; HANDLE NUMBERED LINE
; ----------------------------------------------------------------------------
NUMBERED_LINE:
	ldx	PRGEND		; SQUASH VARIABLE TABLE
	stx	VARTAB
	ldx	PRGEND+1
	stx	VARTAB+1
	jsr	LINGET		; GET LINE #
	jsr	PARSE_INPUT_LINE	; AND PARSE THE INPUT LINE
	sty	EOLPNTR		; SAVE INDEX TO INPUT BUFFER
	jsr	FNDLIN		; IS THIS LINE # ALREADY IN PROGRAM?
	bcc	PUT_NEW_LINE	; NO
	ldy	#1		; YES, SO DELETE IT
	lda	(LOWTR),y	; LOWTR POINTS AT LINE
	sta	INDEX+1		; GET HIGH BYTE OF FORWARD PNTR
	lda	VARTAB
	sta	INDEX
	lda	LOWTR+1
	sta	DEST+1
	lda	LOWTR
	dey
	sbc	(LOWTR),y
	clc
	adc	VARTAB
	sta	VARTAB
	sta	DEST
	lda	VARTAB+1
	adc	#$FF
	sta	VARTAB+1
	sbc	LOWTR+1
	tax
	sec
	lda	LOWTR
	sbc	VARTAB
	tay
	bcs	@1
	inx
	dec	DEST+1
@1:	clc
	adc	INDEX
	bcc	@2
	dec	INDEX+1
	clc
; ----------------------------------------------------------------------------
@2:	lda	(INDEX),y	; MOVE HIGHER LINES OF PROGRAM
	sta	(DEST),y	; DOWN OVER THE DELETED LINE.
	iny
	bne	@2
	inc	INDEX+1
	inc	DEST+1
	dex
	bne	@2


; ----------------------------------------------------------------------------
PUT_NEW_LINE:
	lda	INPUTBUFFER	; ANY CHARACTERS AFTER LINE #?
	beq	FIX_LINKS	; NO, SO NOTHING TO INSERT.
	lda	MEMSIZ		; YES, SO MAKE ROOM AND INSERT LINE
	ldy	MEMSIZ+1	; WIPE STRING AREA CLEAN
	sta	FRETOP
	sty	FRETOP+1
	lda	VARTAB		; SET UP BLTU SUBROUTINE
	sta	HIGHTR		; INSERT NEW LINE.
	adc	EOLPNTR
	sta	HIGHDS
	ldy	VARTAB+1
	sty	HIGHTR+1
	bcc	@1
	iny
@1:	sty	HIGHDS+1
	jsr	BLTU		; MAKE ROOM FOR THE LINE
	lda	LINNUM		; PUT LINE NUMBER IN LINE IMAGE
	ldy	LINNUM+1
	sta	INPUTBUFFER-2
	sty	INPUTBUFFER-1
	lda	STREND
	ldy	STREND+1
	sta	VARTAB
	sty	VARTAB+1
	ldy	EOLPNTR
; ---COPY LINE INTO PROGRAM-------
@2:	lda	INPUTBUFFER-5,y
	dey
	sta	(LOWTR),y
	bne	@2


; ----------------------------------------------------------------------------
; CLEAR ALL VARIABLES
; RE-ESTABLISH ALL FORWARD LINKS
; ----------------------------------------------------------------------------
FIX_LINKS:
	jsr	SETPTRS		; CLEAR ALL VARIABLES
	lda	TXTTAB		; POINT INDEX AT START OF PROGRAM
	ldy	TXTTAB+1
	sta	INDEX
	sty	INDEX+1
	clc
@1:	ldy	#1		; HI-BYTE OF NEXT FORWARD PNTR
	lda	(INDEX),y	; END OF PROGRAM YET?
	bne	@2		; NO, KEEP GOING
	lda	VARTAB		; YES
	sta	PRGEND
	lda	VARTAB+1
	sta	PRGEND+1
	jmp	RESTART
@2:	ldy	#4		; FIND END OF THIS LINE
@3:	iny			; (NOTE MAXIMUM LENGTH < 256)
	lda	(INDEX),y
	bne	@3
	iny			; COMPUTE ADDRESS OF NEXT LINE
	tya
	adc	INDEX
	tax
	ldy	#0		; STORE FORWARD PNTR IN THIS LINE
	sta	(INDEX),y
	lda	INDEX+1
	adc	#0		; (NOTE: THIS CLEARS CARRY)
	iny
	sta	(INDEX),y
	stx	INDEX
	sta	INDEX+1
	bcc	@1		; ...ALWAYS


; ----------------------------------------------------------------------------
; READ A LINE, AND STRIP OFF SIGN BITS
; ----------------------------------------------------------------------------
INLIN:	ldx	#$80		; NULL PROMPT
INLIN2:	stx	PROMPT
	jsr	GETLN
	cpx	#239		; MAXIMUM LINE LENGTH
	bcc	@1
	ldx	#239		; TRUNCATE AT 239 CHARS
@1:	lda	#0		; MARK END OF LINE WITH $00 BYTE
	sta	INPUTBUFFER,x
;	txa
;	beq	@3		; NULL INPUT LINE
;@2:	lda	INPUTBUFFER-1,x	; DROP SIGN BITS
;	and	#$7F		;	already cleared by GETLN
;	sta	INPUTBUFFER-1,x
;	dex
;	bne	@2
;@3:	lda	#0		; (Y,X) POINTS AT BUFFER-1
	ldx	#<INPUTBUFFER-1
	ldy	#>INPUTBUFFER-1
	rts

; ----------------------------------------------------------------------------
; TOKENIZE THE INPUT LINE
; ----------------------------------------------------------------------------
PARSE_INPUT_LINE:
	ldx	TXTPTR		; INDEX INTO UNPARSED LINE
	dex			; PREPARE FOR INX AT "PARSE"
	ldy	#4		; INDEX TO PARSED OUTPUT LINE
	sty	DATAFLG		; CLEAR SIGN-BIT OF DATAFLG
	bit	LOCK		; IS THIS PROGRAM LOCKED?
	bpl	PARSE		; NO, GO AHEAD AND PARSE THE LINE
	pla			; YES, IGNORE INPUT AND "RUN"
	pla			;    THE PROGRAM
	jsr	SETPTRS		; CLEAR ALL VARIABLES
	jmp	NEWSTT		; START RUNNING
; ----------------------------------------------------------------------------
PARSE:	inx			; NEXT INPUT CHARACTER
@1:	lda	INPUTBUFFER,x
	bit	DATAFLG		; IN A "DATA" STATEMENT?
	bvs	@2		; YES (DATAFLG = $49)
	cmp	#' '		; IGNORE BLANKS
	beq	PARSE
@2:	sta	ENDCHR
	cmp	#'"'		; START OF QUOTATION?
	beq	@13
	bvs	@9		; BRANCH IF IN "DATA" STATEMENT
	cmp	#'?'		; SHORTHAND FOR "PRINT"?
	bne	@3		; NO
	lda	#TOKEN_PRINT	; YES, REPLACE WITH "PRINT" TOKEN
	bne	@9		; ...ALWAYS
@3:	cmp	#'0'		; IS IT A DIGIT, COLON, OR SEMI-COLON?
	bcc	@4		; NO, PUNCTUATION !"#$%&'()*+,-./
	cmp	#';'+1
	bcc	@9		; YES, NOT A TOKEN
; ----------------------------------------------------------------------------
; SEARCH TOKEN NAME TABLE FOR MATCH STARTING
; WITH CURRENT CHAR FROM INPUT LINE
; ----------------------------------------------------------------------------
@4:	sty	STRNG2		; SAVE INDEX TO OUTPUT LINE
	lda	#<(TOKEN_NAME_TABLE-$100)
	sta	FAC		; MAKE PNTR FOR SEARCH
	lda	#>(TOKEN_NAME_TABLE-$100)
	sta	FAC+1
	ldy	#0		; USE Y-REG WITH (FAC) TO ADDRESS TABLE
	sty	TKNCNTR		; HOLDS CURRENT TOKEN-$80
	dey			; PREPARE FOR "INY" A FEW LINES DOWN
	stx	TXTPTR		; SAVE POSITION IN INPUT LINE
	dex			; PREPARE FOR "INX" A FEW LINES DOWN
@5:	iny			; ADVANCE POINTER TO TOKEN TABLE
	bne	@6		; Y=Y+1 IS ENOUGH
	inc	FAC+1		; ALSO NEED TO BUMP THE PAGE
@6:	inx			; ADVANCE POINTER TO INPUT LINE
@7:	lda	INPUTBUFFER,x	; NEXT CHAR FROM INPUT LINE
	cmp	#' '		; THIS CHAR A BLANK?
	beq	@6		; YES, IGNORE ALL BLANKS
	sec			; NO, COMPARE TO CHAR IN TABLE
	sbc	(FAC),y		; SAME AS NEXT CHAR OF TOKEN NAME?
	beq	@5		; YES, CONTINUE MATCHING
	cmp	#$80		; MAYBE; WAS IT SAME EXCEPT FOR BIT 7?
	bne	@14		; NO, SKIP TO NEXT TOKEN
	ora	TKNCNTR		; YES, END OF TOKEN; GET TOKEN #
; ----------------------------------------------------------------------------
; STORE CHARACTER OR TOKEN IN OUTPUT LINE
; ----------------------------------------------------------------------------
@8:	ldy	STRNG2		; GET INDEX TO OUTPUT LINE IN Y-REG
@9:	inx			; ADVANCE INPUT INDEX
	iny			; ADVANCE OUTPUT INDEX
	sta	INPUTBUFFER-5,y	; STORE CHAR OR TOKEN
	lda	INPUTBUFFER-5,y	; TEST FOR EOL OR EOS
	beq	@17		; END OF LINE
	sec
	sbc	#':'		; END OF STATEMENT?
	beq	@10		; YES, CLEAR DATAFLG
	cmp	#TOKEN_DATA-':'	; "DATA" TOKEN?
	bne	@11		; NO, LEAVE DATAFLG ALONE
@10:	sta	DATAFLG		; DATAFLG = 0 OR $83-$3A = $49
@11:	sec			; IS IT A "REM" TOKEN?
	sbc	#TOKEN_REM-':'
	bne	@1		; NO, CONTINUE PARSING LINE
	sta	ENDCHR		; YES, CLEAR LITERAL FLAG
; ----------------------------------------------------------------------------
; HANDLE LITERAL (BETWEEN QUOTES) OR REMARK,
; BY COPYING CHARS UP TO ENDCHR.
; ----------------------------------------------------------------------------
@12:	lda	INPUTBUFFER,x
	beq	@9		; END OF LINE
	cmp	ENDCHR
	beq	@9		; FOUND ENDCHR
@13:	iny			; NEXT OUTPUT CHAR
	sta	INPUTBUFFER-5,y
	inx			; NEXT INPUT CHAR
	bne	@12		; ...ALWAYS
; ----------------------------------------------------------------------------
; ADVANCE POINTER TO NEXT TOKEN NAME
; ----------------------------------------------------------------------------
@14:	ldx	TXTPTR		; GET POINTER TO INPUT LINE IN X-REG
	inc	TKNCNTR		;    BUMP (TOKEN # - $80)
@15:	lda	(FAC),y		; SCAN THROUGH TABLE FOR BIT7 = 1
	iny			; NEXT TOKEN ONE BEYOND THAT
	bne	@16		; ...USUALLY ENOUGH TO BUMP Y-REG
	inc	FAC+1		; NEXT SET OF 256 TOKEN CHARS
@16:	asl	a		; SEE IF SIGN BIT SET ON CHAR
	bcc	@15		; NO, MORE IN THIS NAME
	lda	(FAC),y		; YES, AT NEXT NAME.  END OF TABLE?
	bne	@7		; NO, NOT END OF TABLE
	lda	INPUTBUFFER,x	; YES, SO NOT A KEYWORD
	bpl	@8		; ...ALWAYS, COPY CHAR AS IS
; ---END OF LINE------------------
@17:	sta	INPUTBUFFER-3,y	; STORE ANOTHER 00 ON END
	dec	TXTPTR+1	; SET TXTPTR = INPUTBUFFER-1
	lda	#<INPUTBUFFER-1
	sta	TXTPTR
	rts


; ----------------------------------------------------------------------------
; SEARCH FOR LINE
;
; (LINNUM) = LINE # TO FIND
; IF NOT FOUND:  CARRY = 0
;	LOWTR POINTS AT NEXT LINE
; IF FOUND:      CARRY = 1
;	LOWTR POINTS AT LINE
; ----------------------------------------------------------------------------
FNDLIN:	lda	TXTTAB		; SEARCH FROM BEGINNING OF PROGRAM
	ldx	TXTTAB+1
FL1:	ldy	#1		; SEARCH FROM (X,A)
	sta	LOWTR
	stx	LOWTR+1
	lda	(LOWTR),y
	beq	@3		; END OF PROGRAM, AND NOT FOUND
	iny
	iny
	lda	LINNUM+1
	cmp	(LOWTR),y
	bcc	RTS1		; IF NOT FOUND
	beq	@1
	dey
	bne	@2
@1:	lda	LINNUM
	dey
	cmp	(LOWTR),y
	bcc	RTS1		; PAST LINE, NOT FOUND
	beq	RTS1		; IF FOUND
@2:	dey
	lda	(LOWTR),y
	tax
	dey
	lda	(LOWTR),y
	bcs	FL1		; ALWAYS
@3:	clc			; RETURN CARRY = 0
RTS1:	rts


; ----------------------------------------------------------------------------
; "NEW" STATEMENT
; ----------------------------------------------------------------------------
NEW:	bne	RTS1		; IGNORE IF MORE TO THE STATEMENT
SCRTCH:	lda	#0
	sta	LOCK
	tay
	sta	(TXTTAB),y
	iny
	sta	(TXTTAB),y
	lda	TXTTAB
	adc	#2		; (CARRY WASN'T CLEARED, SO "NEW" USUALLY
	sta	VARTAB		; ADDS 3, WHEREAS "FP" ADDS 2.)
	sta	PRGEND
	lda	TXTTAB+1
	adc	#0
	sta	VARTAB+1
	sta	PRGEND+1
; ----------------------------------------------------------------------------
SETPTRS:
	jsr	STXTPT		; SET TXTPTR TO TXTTAB - 1
	lda	#0		; (THIS COULD HAVE BEEN ".byte $2C")


; ----------------------------------------------------------------------------
; "CLEAR" STATEMENT
; ----------------------------------------------------------------------------
CLEAR:	bne	RTS2		; IGNORE IF NOT AT END OF STATEMENT
CLEARC:	lda	MEMSIZ		; CLEAR STRING AREA
	ldy	MEMSIZ+1
	sta	FRETOP
	sty	FRETOP+1
	lda	VARTAB		; CLEAR ARRAY AREA
	ldy	VARTAB+1
	sta	ARYTAB
	sty	ARYTAB+1
	sta	STREND		; LOW END OF FREE SPACE
	sty	STREND+1
	jsr	RESTORE		; SET "DATA" POINTER TO BEGINNING
; ----------------------------------------------------------------------------
STKINI:	ldx	#TEMPST
	stx	TEMPPT
	pla			; SAVE RETURN ADDRESS
	tay
	pla
	ldx	#$F8		; START STACK AT $F8,
	txs			;   LEAVING ROOM FOR PARSING LINES
	pha			; RESTORE RETURN ADDRESS
	tya
	pha
	lda	#0
	sta	OLDTEXT+1
	sta	SUBFLG
RTS2:	rts

; ----------------------------------------------------------------------------
; SET TXTPTR TO BEGINNING OF PROGRAM
; ----------------------------------------------------------------------------
STXTPT:	clc			; TXTPTR = TXTTAB - 1
	lda     TXTTAB
	adc     #$FF
	sta     TXTPTR
	lda     TXTTAB+1
	adc     #$FF
	sta     TXTPTR+1
	rts

; ----------------------------------------------------------------------------
; "LIST" STATEMENT
; ----------------------------------------------------------------------------
LIST:	bcc	@1		; NO  LINE # SPECIFIED
	beq	@1		; ---DITTO---
	cmp	#TOKEN_MINUS	; IF DASH OR COMMA, START AT LINE 0
	beq	@1		; IT IS A DASH
	cmp	#','		; COMMA?
	bne	RTS2		; NO, ERROR
@1:	jsr	LINGET		; CONVERT LINE NUMBER IF ANY
	jsr	FNDLIN		; POINT LOWTR TO 1ST LINE
	jsr	CHRGOT		; RANGE SPECIFIED?
	beq	@3		; NO
	cmp	#TOKEN_MINUS
	beq	@2
	cmp	#','
	bne	RTS1
@2:	jsr	CHRGET		; GET NEXT CHAR
	jsr	LINGET		; CONVERT SECOND LINE #
	bne	RTS2		; BRANCH IF SYNTAX ERR
@3:	pla			; POP RETURN ADRESS
	pla			; (GET BACK BY "JMP NEWSTT")
	lda	LINNUM		; IF NO SECOND NUMBER, USE $FFFF
	ora	LINNUM+1
	bne	LIST0		; THERE WAS A SECOND NUMBER
	lda	#$FF		; MAX END RANGE
	sta	LINNUM
	sta	LINNUM+1
LIST0:	ldy	#1
	lda	(LOWTR),y	; HIGH BYTE OF LINK
	beq	LIST3		; END OF PROGRAM
	jsr	ISCNTC		; CHECK IF CONTROL-C HAS BEEN TYPED
	jsr	CRDO		; NO, PRINT <RETURN>
	iny
	lda	(LOWTR),y	; GET LINE #, COMPARE WITH END RANGE
	tax
	iny
	lda	(LOWTR),y
	cmp	LINNUM+1
	bne	@5
	cpx	LINNUM
	beq	@6		; ON LAST LINE OF RANGE
@5:	bcs	LIST3		; FINISHED THE RANGE
; ---LIST ONE LINE----------------
@6:	sty	FORPNT
	jsr	LINPRT		; PRINT LINE # FROM X,A
	lda	#' '		; PRINT SPACE AFTER LINE #
LIST1:	ldy	FORPNT
	and	#$7F
LIST2:	jsr	OUTDO
@1:	iny
	lda	(LOWTR),y
	bne	LIST4		; NOT END OF LINE YET
	tay			; END OF LINE
	lda	(LOWTR),y	; GET LINK TO NEXT LINE
	tax
	iny
	lda	(LOWTR),y
	stx	LOWTR		; POINT TO NEXT LINE
	sta	LOWTR+1
	bne	LIST0		; BRANCH IF NOT END OF PROGRAM
LIST3:	jsr	CRDO		; PRINT <RETURN>
	jmp	NEWSTT		; TO NEXT STATEMENT
; ----------------------------------------------------------------------------
GETCHR:	iny			;  PICK UP CHAR FROM TABLE
	bne	@1
	inc	FAC+1
@1:	lda	(FAC),y
	rts
; ----------------------------------------------------------------------------
LIST4:	bpl	LIST2		; BRANCH IF NOT A TOKEN
	sec
	sbc	#$7F		; CONVERT TOKEN TO INDEX
	tax
	sty	FORPNT		; SAVE LINE POINTER
	ldy	#<(TOKEN_NAME_TABLE-$100)
	sty	FAC		; POINT FAC TO TABLE
	ldy	#>(TOKEN_NAME_TABLE-$100)
	sty	FAC+1
	ldy	#-1
@1:	dex			; SKIP KEYWORDS UNTIL REACH THIS ONE
	beq	@3
@2:	jsr	GETCHR		; BUMP Y, GET CHAR FROM TABLE
	bpl	@2		; NOT AT END OF KEYWORD YET
	bmi	@1		; END OF KEYWORD, ALWAYS BRANCHES
@3:	jsr	OUTSP		; FOUND THE RIGHT KEYWORD, PRINT LEADING SPACE
@4:	jsr	GETCHR		; PRINT THE KEYWORD
	bmi	@5		; LAST CHAR OF KEYWORD
	jsr	OUTDO
	bne	@4		; ...ALWAYS
@5:	jsr	OUTDO		; PRINT LAST CHAR OF KEYWORD
	lda	#' '		; PRINT TRAILING SPACE
	bne	LIST1		; ...ALWAYS, BACK TO ACTUAL LINE


; ----------------------------------------------------------------------------
; "FOR" STATEMENT
;
; FOR PUSHES 18 BYTES ON THE STACK:
; 2 -- TXTPTR
; 2 -- LINE NUMBER
; 5 -- INITIAL (CURRENT)  FOR VARIABLE VALUE
; 1 -- STEP SIGN
; 5 -- STEP VALUE
; 2 -- ADDRESS OF FOR VARIABLE IN VARTAB
; 1 -- FOR TOKEN ($81)
; ----------------------------------------------------------------------------
FOR:	lda	#$80
	sta	SUBFLG		; SUBSCRIPTS NOT ALLOWED
	jsr	LET		; DO <VAR> = <EXP>, STORE ADDR IN FORPNT
	jsr	GTFORPNT	; IS THIS FOR VARIABLE ACTIVE?
	bne	@1		; NO
	txa			; YES, CANCEL IT AND ENCLOSED LOOPS
	adc	#15		; CARRY=1, THIS ADDS 16
	tax			; X WAS ALREADY S+2
	txs
@1:	pla			; POP RETURN ADDRESS TOO
	pla
	lda	#9		; BE CERTAIN ENOUGH ROOM IN STACK
	jsr	CHKMEM
	jsr	DATAN		; SCAN AHEAD TO NEXT STATEMENT
	clc			; PUSH STATEMENT ADDRESS ON STACK
	tya
	adc	TXTPTR
	pha
	lda	TXTPTR+1
	adc	#0
	pha
	lda	CURLIN+1	; PUSH LINE NUMBER ON STACK
	pha
	lda	CURLIN
	pha
	lda	#TOKEN_TO
	jsr	SYNCHR		; REQUIRE "TO"
	jsr	CHKNUM		; <VAR> = <EXP> MUST BE NUMERIC
	jsr	FRMNUM		; GET FINAL VALUE, MUST BE NUMERIC
	lda	FACSIGN		; PUT SIGN INTO VALUE IN FAC
	ora	#$7F
	and	FAC+1
	sta	FAC+1
	lda	#<STEP		; SET UP FOR RETURN
	ldy	#>STEP		; TO STEP
	sta	INDEX
	sty	INDEX+1
	jmp	FRM_STACK3	; RETURNS BY "JMP (INDEX)"


; ----------------------------------------------------------------------------
; "STEP" PHRASE OF "FOR" STATEMENT
; ----------------------------------------------------------------------------
STEP:	lda	#<CON_ONE	; STEP DEFAULT=1
	ldy	#>CON_ONE
	jsr	LOAD_FAC_FROM_YA
	jsr	CHRGOT
	cmp	#TOKEN_STEP
	bne	@1		; USE DEFAULT VALUE OF 1.0
	jsr	CHRGET		; STEP SPECIFIED, GET IT
	jsr	FRMNUM
@1:	jsr	SIGN
	jsr	FRM_STACK2
	lda	FORPNT+1
	pha
	lda	FORPNT
	pha
	lda	#TOKEN_FOR
	pha


; ----------------------------------------------------------------------------
; PERFORM NEXT STATEMENT
; ----------------------------------------------------------------------------
NEWSTT:	tsx			; REMEMBER THE STACK POSITION
	stx	REMSTK
	jsr	ISCNTC		; SEE IF CONTROL-C HAS BEEN TYPED
	lda	TXTPTR		; NO, KEEP EXECUTING
	ldy	TXTPTR+1
	ldx	CURLIN+1	; =$FF IF IN DIRECT MODE
	inx			; $FF TURNS INTO $00
	beq	@1		; IN DIRECT MODE
	sta	OLDTEXT		; IN RUNNING MODE
	sty	OLDTEXT+1
@1:	ldy	#0
	lda	(TXTPTR),y	; END OF LINE YET?
	bne	COLON		; NO
	ldy	#2		; YES, SEE IF END OF PROGRAM
	lda	(TXTPTR),y
	clc
	beq	GOEND		; YES, END OF PROGRAM
	iny
	lda	(TXTPTR),y	; GET LINE # OF NEXT LINE
	sta	CURLIN
	iny
	lda	(TXTPTR),y
	sta	CURLIN+1
	tya			; ADJUST TXTPTR TO START
	adc	TXTPTR		; OF NEW LINE
	sta	TXTPTR
	bcc	@2
	inc	TXTPTR+1
@2:
NEWSTT2:
	jsr	CHRGET		; GET FIRST CHR OF STATEMENT
	jsr	EXECUTE_STATEMENT	; AND START PROCESSING
	jmp	NEWSTT		; BACK FOR MORE
; ----------------------------------------------------------------------------
GOEND:	beq	END4


; ----------------------------------------------------------------------------
; EXECUTE A STATEMENT
;
; (A) IS FIRST CHAR OF STATEMENT
; CARRY IS SET
; ----------------------------------------------------------------------------
EXECUTE_STATEMENT:
	beq	RTS3		; END OF LINE, NULL STATEMENT
EXECUTE_STATEMENT1:
	sbc	#$80		; FIRST CHAR A TOKEN?
	bcc	@1		; NOT TOKEN, MUST BE "LET"
	cmp	#$40		; STATEMENT-TYPE TOKEN?
	bcs	SYNERR1		; NO, SYNTAX ERROR
	asl	a		; DOUBLE TO GET INDEX
	tay			; INTO ADDRESS TABLE
	lda	TOKEN_ADDRESS_TABLE+1,y
	pha			; PUT ADDRESS ON STACK
	lda	TOKEN_ADDRESS_TABLE,y
	pha
	jmp	CHRGET		; GET NEXT CHR & RTS TO ROUTINE
; ----------------------------------------------------------------------------
@1:	jmp	LET		; MUST BE <VAR> = <EXP>
; ----------------------------------------------------------------------------
COLON:	cmp	#':'
	beq	NEWSTT2
SYNERR1:
	jmp	SYNERR


; ----------------------------------------------------------------------------
; "RESTORE" STATEMENT
; ----------------------------------------------------------------------------
RESTORE:
	sec			; SET DATPTR TO BEGINNING OF PROGRAM
	lda	TXTTAB
	sbc	#1
	ldy	TXTTAB+1
	bcs	SETDA
	dey
; ---SET DATPTR TO Y,A------------
SETDA:  sta	DATPTR
	sty	DATPTR+1
RTS3:   rts


; ----------------------------------------------------------------------------
; SEE IF CONTROL-C TYPED
; ----------------------------------------------------------------------------
ISCNTC:	lda	KEYBOARD
	cmp	#$83
	beq	@1
	rts
@1:	jsr	RDKEY
CONTROL_C_TYPED:
	ldx	#$FF		; CONTROL C ATTEMPTED
	bit	ERRFLG		; "ON ERR" ENABLED?
	bpl	@2		; NO
	jmp	HANDLERR	; YES, RETURN ERR CODE = 255
@2:	cmp	#3		; SINCE IT IS CTRL-C, SET Z AND C BITS


; ----------------------------------------------------------------------------
; "STOP" STATEMENT
; ----------------------------------------------------------------------------
STOP:	bcs	END2		; CARRY=1 TO FORCE PRINTING "BREAK AT.."


; ----------------------------------------------------------------------------
; "END" STATEMENT
; ----------------------------------------------------------------------------
END:	clc			; CARRY=0 TO AVOID PRINTING MESSAGE
END2:	bne     RTS4		; IF NOT END OF STATEMENT, DO NOTHING
	lda     TXTPTR
	ldy     TXTPTR+1
	ldx     CURLIN+1
	inx			; RUNNING?
	beq     @1		; NO, DIRECT MODE
	sta     OLDTEXT
	sty     OLDTEXT+1
	lda     CURLIN
	ldy     CURLIN+1
	sta     OLDLIN
	sty     OLDLIN+1
@1:	pla
	pla
END4:   lda     #<QT_BREAK	; " BREAK"
	ldy     #>QT_BREAK
	bcc     @1
	jmp     PRINT_ERROR_LINNUM
@1:	jmp	RESTART


; ----------------------------------------------------------------------------
; "CONT" COMMAND
; ----------------------------------------------------------------------------
CONT:	bne	RTS4		; IF NOT END OF STATEMENT, DO NOTHING
	ldx	#ERR_CANTCONT
	ldy	OLDTEXT+1	; MEANINGFUL RE-ENTRY?
	bne	@1		; YES
	jmp	ERROR		; NO
@1:	lda	OLDTEXT		; RESTORE TXTPTR
	sta	TXTPTR
	sty	TXTPTR+1
	lda	OLDLIN		; RESTORE LINE NUMBER
	ldy	OLDLIN+1
	sta	CURLIN
	sty	CURLIN+1
RTS4:	rts


; ----------------------------------------------------------------------------
; "RUN" COMMAND
; ----------------------------------------------------------------------------
RUN:	php			; SAVE STATUS WHILE SUBTRACTING
	dec	CURLIN+1	; IF WAS $FF (MEANING DIRECT MODE) MAKE IT "RUNNING MODE"
	plp			; GET STATUS AGAIN (FROM CHRGET)
	bne	@1		; PROBABLY A LINE NUMBER
	jmp	SETPTRS		; START AT BEGINNING OF PROGRAM
@1:	jsr	CLEARC		; CLEAR VARIABLES
	jmp	GO_TO_LINE	; JOIN GOSUB STATEMENT


; ----------------------------------------------------------------------------
; "GOSUB" STATEMENT
;
; LEAVES 7 BYTES ON STACK:
; 2 -- RETURN ADDRESS (NEWSTT)
; 2 -- TXTPTR
; 2 -- LINE #
; 1 -- GOSUB TOKEN
; ----------------------------------------------------------------------------
GOSUB:	lda	#3		; BE SURE ENOUGH ROOM ON STACK
	jsr	CHKMEM
	lda	TXTPTR+1
	pha
	lda	TXTPTR
	pha
	lda	CURLIN+1
	pha
	lda	CURLIN
	pha
	lda	#TOKEN_GOSUB
	pha
GO_TO_LINE:
	jsr	CHRGOT
	jsr	GOTO
	jmp	NEWSTT


; ----------------------------------------------------------------------------
; "GOTO" STATEMENT
; ALSO USED BY "RUN" AND "GOSUB"
; ----------------------------------------------------------------------------
GOTO:	jsr	LINGET		; GET GOTO LINE
	jsr	REMN		; POINT Y TO EOL
	lda	CURLIN+1	; IS CURRENT PAGE < GOTO PAGE?
	cmp	LINNUM+1
	bcs	@1		; SEARCH FROM PROG START IF NOT
	tya			; OTHERWISE SEARCH FROM NEXT LINE
	sec
	adc	TXTPTR
	ldx	TXTPTR+1
	bcc	@2
	inx
	bcs	@2
@1:	lda	TXTTAB		; GET PROGRAM BEGINNING
	ldx	TXTTAB+1
@2:	jsr	FL1		; SEARCH FOR GOTO LINE
	bcc	UNDERR		; ERROR IF NOT THERE
	lda	LOWTR		; TXTPTR = START OF THE DESTINATION LINE
	sbc	#1
	sta	TXTPTR
	lda	LOWTR+1
	sbc	#0
	sta	TXTPTR+1
RTS5:	rts			; RETURN TO NEWSTT OR GOSUB


; ----------------------------------------------------------------------------
; "POP" AND "RETURN" STATEMENTS
; ----------------------------------------------------------------------------
POP:	bne	RTS5
	lda	#$FF
	sta	FORPNT		; <<< BUG: SHOULD BE FORPNT+1  SEE "ALL ABOUT APPLESOFT", PAGES 100,101>>>
	jsr	GTFORPNT	; TO CANCEL FOR/NEXT IN SUB
	txs
	cmp	#TOKEN_GOSUB	; LAST GOSUB FOUND?
	beq	RETURN
	ldx	#ERR_NOGOSUB
	.byte	$2C		; FAKE
UNDERR:	ldx	#ERR_UNDEFSTAT
	jmp	ERROR
; ----------------------------------------------------------------------------
SYNERR2:
	jmp	SYNERR
; ----------------------------------------------------------------------------
RETURN:	pla			; DISCARD GOSUB TOKEN
	pla
	cpy	#<(TOKEN_POP*2)	; BRANCH IF A POP
	beq	PULL3		; PULL LINE #
	sta	CURLIN
	pla
	sta	CURLIN+1
	pla
	sta	TXTPTR		; PULL TXTPTR
	pla
	sta	TXTPTR+1


; ----------------------------------------------------------------------------
; "DATA" STATEMENT
; EXECUTED BY SKIPPING TO NEXT COLON OR EOL
; ----------------------------------------------------------------------------
DATA:	jsr	DATAN		; MOVE TO NEXT STATEMENT


; ----------------------------------------------------------------------------
; ADD (Y) TO TXTPTR
; ----------------------------------------------------------------------------
ADDON:	tya
	clc
	adc	TXTPTR
	sta	TXTPTR
	bcc	@1
	inc	TXTPTR+1
@1:
RTS6:	rts


; ----------------------------------------------------------------------------
; SCAN AHEAD TO NEXT ":" OR EOL
; ----------------------------------------------------------------------------
DATAN:	ldx	#':'		; GET OFFSET IN Y TO EOL OR ":"
	.byte	$2C		; FAKE
; ----------------------------------------------------------------------------
REMN:	ldx	#0		; TO EOL ONLY
	stx	CHARAC
	ldy	#0
	sty	ENDCHR
@1:	lda	ENDCHR		; TRICK TO COUNT QUOTE PARITY
	ldx	CHARAC
	sta	CHARAC
	stx	ENDCHR
@2:	lda	(TXTPTR),y
	beq	RTS6		; END OF LINE
	cmp 	ENDCHR
	beq	RTS6		; COLON IF LOOKING FOR COLONS
	iny
	cmp	#'"'
	bne	@2
	beq	@1		; ...ALWAYS
; ----------------------------------------------------------------------------
PULL3:	pla
	pla
	pla
	rts


; ----------------------------------------------------------------------------
; "IF" STATEMENT
; ----------------------------------------------------------------------------
IF:	jsr	FRMEVL
	jsr	CHRGOT
	cmp	#TOKEN_GOTO
	beq	@1
	lda	#TOKEN_THEN
	jsr	SYNCHR
@1:	lda	FAC		; CONDITION TRUE OR FALSE?
	bne	IF_TRUE		; BRANCH IF TRUE


; ----------------------------------------------------------------------------
; "REM" STATEMENT, OR FALSE "IF" STATEMENT
; ----------------------------------------------------------------------------
REM:	jsr	REMN		; SKIP REST OF LINE
	beq	ADDON		; ...ALWAYS
; ----------------------------------------------------------------------------
IF_TRUE:
	jsr	CHRGOT		; COMMAND OR NUMBER?
	bcs	@1		; COMMAND
	jmp	GOTO		; NUMBER
@1:	jmp	EXECUTE_STATEMENT


; ----------------------------------------------------------------------------
; "ON" STATEMENT
;
; ON <EXP> GOTO <LIST>
; ON <EXP> GOSUB <LIST>
; ----------------------------------------------------------------------------
ONGOTO:	jsr	GETBYT		; EVALUATE <EXP>, AS BYTE IN FAC+4
	pha			; SAVE NEXT CHAR ON STACK
	cmp	#TOKEN_GOSUB
	beq	ON2
ON1:	cmp	#TOKEN_GOTO
	bne	SYNERR2
ON2:	dec	FAC+4		; COUNTED TO RIGHT ONE YET?
	bne	@3		; NO, KEEP LOOKING
	pla			; YES, RETRIEVE CMD
	jmp	EXECUTE_STATEMENT1	; AND GO.
@3:	jsr	CHRGET		; PRIME CONVERT SUBROUTINE
	jsr	LINGET		; CONVERT LINE #
	cmp	#','		; TERMINATE WITH COMMA?
	beq	ON2		; YES
	pla			; NO, END OF LIST, SO IGNORE
RTS7:	rts


; ----------------------------------------------------------------------------
; CONVERT LINE NUMBER
; ----------------------------------------------------------------------------
LINGET:	ldx	#0		; ASC # TO HEX ADDRESS
	stx	LINNUM		; IN LINNUM.
	stx	LINNUM+1
@1:	bcs	RTS7		; NOT A DIGIT
	sbc	#'0'-1		; CONVERT DIGIT TO BINARY
	sta	CHARAC		; SAVE THE DIGIT
	lda	LINNUM+1	; CHECK RANGE
	sta	INDEX
	cmp	#>6400		; LINE # TOO LARGE?
	bcs	ON1		; YES, > 63999, GO INDIRECTLY TO "SYNTAX ERROR".
	lda	LINNUM		; MULTIPLY BY TEN
	asl	a
	rol	INDEX
	asl	a
	rol	INDEX
	adc	LINNUM
	sta	LINNUM
	lda	INDEX
	adc	LINNUM+1
	sta	LINNUM+1
	asl	LINNUM
	rol	LINNUM+1
	lda	LINNUM
	adc	CHARAC		; ADD DIGIT
	sta	LINNUM
	bcc	@2
	inc	LINNUM+1
@2:	jsr	CHRGET		; GET NEXT CHAR
	jmp	@1		; MORE CONVERTING


; ----------------------------------------------------------------------------
; "LET" STATEMENT
;
; LET <VAR> = <EXP>
; <VAR> = <EXP>
; ----------------------------------------------------------------------------
LET:	jsr	PTRGET		; GET <VAR>
	sta	FORPNT
	sty	FORPNT+1
	lda	#TOKEN_EQUAL
	jsr	SYNCHR
	lda	VALTYP+1	; SAVE VARIABLE TYPE
	pha
	lda	VALTYP
	pha
	jsr	FRMEVL		; EVALUATE <EXP>
	pla
	rol	a
	jsr	CHKVAL
	bne	LETSTRING
	pla
; ----------------------------------------------------------------------------
LET2:	bpl	@1		; REAL VARIABLE
	jsr	ROUND_FAC	; INTEGER VAR: ROUND TO 32 BITS
	jsr	AYINT		; TRUNCATE TO 16-BITS
	ldy	#0
	lda	FAC+3
	sta	(FORPNT),y
	iny
	lda	FAC+4
	sta	(FORPNT),y
	rts


; ----------------------------------------------------------------------------
; REAL VARIABLE = EXPRESSION
; ----------------------------------------------------------------------------
@1:	jmp	SETFOR
; ----------------------------------------------------------------------------
LETSTRING:
	pla


; ----------------------------------------------------------------------------
; INSTALL STRING, DESCRIPTOR ADDRESS IS AT FAC+3,4
; ----------------------------------------------------------------------------
PUTSTR:	ldy	#2		; STRING DATA ALREADY IN STRING AREA?
	lda	(FAC+3),y	; (STRING AREA IS BTWN FRETOP
	cmp	FRETOP+1	; 	HIMEM)
	bcc	@2		; YES, DATA ALREADY UP THERE
	bne	@1		; NO
	dey			; MAYBE, TEST LOW BYTE OF POINTER
	lda	(FAC+3),y
	cmp	FRETOP
	bcc	@2		; YES, ALREADY THERE
@1:	ldy	FAC+4		; NO. DESCRIPTOR ALREADY AMONG VARIABLES?
	cpy	VARTAB+1
	bcc	@2		; NO
	bne	@3		; YES
	lda	FAC+3		; MAYBE, COMPARE LO-BYTE
	cmp	VARTAB
	bcs	@3		; YES, DESCRIPTOR IS AMONG VARIABLES
@2:	lda	FAC+3		; EITHER STRING ALREADY ON TOP, OR
	ldy	FAC+4		; DESCRIPTOR IS NOT A VARIABLE
	jmp	@4		; SO JUST STORE THE DESCRIPTOR
; ----------------------------------------------------------------------------
; STRING NOT YET IN STRING AREA,
; AND DESCRIPTOR IS A VARIABLE
; ----------------------------------------------------------------------------
@3:	ldy	#0		; POINT AT LENGTH IN DESCRIPTOR
	lda	(FAC+3),y	; GET LENGTH
	jsr	STRINI		; MAKE A STRING THAT LONG UP ABOVE
	lda	DSCPTR		; SET UP SOURCE PNTR FOR MONINS
	ldy	DSCPTR+1
	sta	STRNG1
	sty	STRNG1+1
	jsr	MOVINS		; MOVE STRING DATA TO NEW AREA
	lda	#<FAC		; ADDRESS OF DESCRIPTOR IS IN FAC
	ldy	#>FAC
@4:	sta	DSCPTR
	sty	DSCPTR+1
	jsr	FRETMS		; DISCARD DESCRIPTOR IF 'TWAS TEMPORARY
	ldy	#0		; COPY STRING DESCRIPTOR
	lda	(DSCPTR),y
	sta	(FORPNT),y
	iny
	lda	(DSCPTR),y
	sta	(FORPNT),y
	iny
	lda	(DSCPTR),y
	sta	(FORPNT),y
RTS8:	rts


; ----------------------------------------------------------------------------
PRSTRING:
	jsr	STRPRT
	jsr	CHRGOT


; ----------------------------------------------------------------------------
; "PRINT" STATEMENT
; ----------------------------------------------------------------------------
PRINT:	beq	GOCR		; NO MORE LIST, PRINT <RETURN>
; ----------------------------------------------------------------------------
PRINT2: beq	RTS8		; NO MORE LIST, DON'T PRINT <RETURN>
	cmp	#TOKEN_SPC
	clc
	beq	PR_TAB_OR_SPC	; C=0 FOR SPC(
	cmp	#','
	beq	PR_NEXT_CHAR
	cmp	#';'
	beq	PR_NEXT_CHAR
	jsr	FRMEVL		; EVALUATE EXPRESSION
	bit	VALTYP		; STRING OR FP VALUE?
	bmi	PRSTRING	; STRING
	jsr	FOUT		; FP: CONVERT INTO BUFFER
	jsr	STRLIT		; 	MAKE BUFFER INTO STRING
	jmp	PRSTRING	; 	PRINT THE STRING
; ----------------------------------------------------------------------------
GOCR:	jmp	CRDO

; ----------------------------------------------------------------------------
PR_TAB_OR_SPC:
	jsr	GTBYTC		; GET VALUE
	cmp	#')'		; TRAILING PARENTHESIS
	beq	@2		; GOOD
	jmp	SYNERR		; NO, SYNTAX ERROR
@2:	inx
NXSPC:  dex
	bne	DOSPC		; MORE SPACES TO PRINT
; ----------------------------------------------------------------------------
PR_NEXT_CHAR:
	jsr	CHRGET
	jmp	PRINT2		; CONTINUE PARSING PRINT LIST
; ----------------------------------------------------------------------------
DOSPC:	jsr	OUTSP
	bne	NXSPC		; ...ALWAYS


; ----------------------------------------------------------------------------
; PRINT STRING AT (Y,A)
; ----------------------------------------------------------------------------
STROUT:	jsr	STRLIT		; MAKE (Y,A) PRINTABLE


; ----------------------------------------------------------------------------
; PRINT STRING AT (FACMO,FACLO)
; ----------------------------------------------------------------------------
STRPRT:	jsr	FREFAC		; GET ADDRESS INTO INDEX, (A)=LENGTH
	tax			; USE X-REG FOR COUNTER
	ldy	#0		; USE Y-REG FOR SCANNER
	inx
@1:	dex
	beq	RTS8		; FINISHED
	lda	(INDEX),y	; NEXT CHAR FROM STRING
	jsr	OUTDO		; PRINT THE CHAR
	iny
	jmp	@1


; ----------------------------------------------------------------------------
; INPUT CONVERSION ERROR:  ILLEGAL CHARACTER
; IN NUMERIC FIELD.  MUST DISTINGUISH
; BETWEEN INPUT, READ, AND GET
; ----------------------------------------------------------------------------
INPUTERR:
	lda	INPUTFLG
	beq	RESPERR		; TAKEN IF INPUT
	bmi	READERR		; TAKEN IF READ
	ldy	#$FF		; FROM A GET
	bne	ERLIN		;  ...ALWAYS
; ----------------------------------------------------------------------------
READERR:
	lda	DATLIN		; TELL WHERE THE "DATA" IS, RATHER
	ldy	DATLIN+1	; THAN THE "READ"
; ----------------------------------------------------------------------------
ERLIN:	sta	CURLIN
	sty	CURLIN+1
	jmp	SYNERR
; ----------------------------------------------------------------------------
INPERR:	pla
; ----------------------------------------------------------------------------
RESPERR:
	bit	ERRFLG		; "ON ERR" TURNED ON?
	bpl	@1		; NO, GIVE REENTRY A TRY
	ldx	#254		; ERROR CODE = 254
	jmp	HANDLERR
@1:	lda	#<ERRREENTRY	; "?REENTER"
	ldy	#>ERRREENTRY
	jsr	STROUT
	lda	OLDTEXT		; RE-EXECUTE THE WHOLE INPUT STATEMENT
	ldy	OLDTEXT+1
	sta	TXTPTR
	sty	TXTPTR+1
	rts


; ----------------------------------------------------------------------------
; "GET" STATEMENT
; ----------------------------------------------------------------------------
GET:	jsr	ERRDIR		; ILLEGAL IF IN DIRECT MODE
	ldx	#<(INPUTBUFFER+1)	; SIMULATE INPUT
	ldy	#>(INPUTBUFFER+1)
	lda	#0
	sta	INPUTBUFFER+1
	lda	#$40		; SET UP INPUTFLG
	jmp	PROCESS_INPUT_LIST


; ----------------------------------------------------------------------------
; "INPUT" STATEMENT
; ----------------------------------------------------------------------------
INPUT:	cmp	#'"'		; CHECK FOR OPTIONAL PROMPT STRING
	bne	@1		; NO, PRINT "?" PROMPT
	jsr	STRTXT		; MAKE A PRINTABLE STRING OUT OF IT
	lda	#';'		; MUST HAVE ; NOW
	jsr	SYNCHR
	jsr	STRPRT		; PRINT THE STRING
	jmp	@2
@1:	jsr	OUTQUES		; NO STRING, PRINT "?"
@2:	jsr	ERRDIR		; ILLEGAL IF IN DIRECT MODE
	lda	#','		; PRIME THE BUFFER
	sta	INPUTBUFFER-1
	jsr	INLIN
	lda	INPUTBUFFER
	cmp	#$03		; CONTROL C?
	bne	INPUTFLAGZERO	; NO
	jmp	CONTROL_C_TYPED
; ----------------------------------------------------------------------------
NXIN:	jsr	OUTQUES		; PRINT "?"
	jmp	INLIN


; ----------------------------------------------------------------------------
; "READ" STATEMENT
; ----------------------------------------------------------------------------
READ:	ldx	DATPTR		; Y,X POINTS AT NEXT DATA STATEMENT
	ldy	DATPTR+1
	lda	#$98		; SET INPUTFLG = $98
	.byte	$2C		; TRICK TO PROCESS_INPUT_LIST
; ----------------------------------------------------------------------------
INPUTFLAGZERO:
	lda	#0		; SET INPUTFLG = $00


; ----------------------------------------------------------------------------
; PROCESS INPUT LIST
;
; (Y,X) IS ADDRESS OF INPUT DATA STRING
; (A) = VALUE FOR INPUTFLG:  $00 FOR INPUT
; 				$40 FOR GET
;				$98 FOR READ
; ----------------------------------------------------------------------------
PROCESS_INPUT_LIST:
	sta	INPUTFLG
	stx	INPTR		; ADDRESS OF INPUT STRING
	sty	INPTR+1

; ----------------------------------------------------------------------------
PROCESS_INPUT_ITEM:
	jsr	PTRGET		; GET ADDRESS OF VARIABLE
	sta	FORPNT
	sty	FORPNT+1
	lda	TXTPTR		; SAVE CURRENT TXTPTR,
	ldy	TXTPTR+1	; WHICH POINTS INTO PROGRAM
	sta	TXPSV
	sty	TXPSV+1
	ldx	INPTR		; SET TXTPTR TO POINT AT INPUT BUFFER
	ldy	INPTR+1		; OR "DATA" LINE
	stx	TXTPTR
	sty	TXTPTR+1
	jsr	CHRGOT		; GET CHAR AT PNTR
	bne	INSTART		; NOT END OF LINE OR COLON
	bit	INPUTFLG	; DOING A "GET"?
	bvc	@1		; NO
	jsr	RDKEY		; YES, GET CHAR
	sta	INPUTBUFFER
	ldx	#<(INPUTBUFFER-1)
	ldy	#>(INPUTBUFFER-1)
	bne	@2		; ...ALWAYS
; ----------------------------------------------------------------------------
@1:	bmi	FINDATA		; DOING A "READ"
	jsr	OUTQUES		; DOING AN "INPUT", PRINT "?"
	jsr	NXIN		; PRINT ANOTHER "?", AND INPUT A LINE
@2:	stx	TXTPTR
	sty	TXTPTR+1

; ----------------------------------------------------------------------------
INSTART:
	jsr	CHRGET		; GET NEXT INPUT CHAR
	bit	VALTYP		; STRING OR NUMERIC?
	bpl	@5		; NUMERIC
	bit	INPUTFLG	; STRING -- NOW WHAT INPUT TYPE?
	bvc	@1		; NOT A "GET"
	inx			; "GET"
	stx	TXTPTR
	lda	#0
	sta	CHARAC		; NO OTHER TERMINATORS THAN $00
	beq	@2		; ...ALWAYS
; ----------------------------------------------------------------------------
@1:	sta	CHARAC
	cmp	#'"'		; TERMINATE ON $00 OR QUOTE
	beq	@3
	lda	#':'		; TERMINATE ON $00, COLON, OR COMMA
	sta	CHARAC
	lda	#','
@2:	clc
@3:	sta	ENDCHR
	lda	TXTPTR
	ldy	TXTPTR+1
	adc	#0		; SKIP OVER QUOTATION MARK, IF
	bcc	@4		;    THERE WAS ONE
	iny
@4:	jsr	STRLT2		; BUILD STRING STARTING AT (Y,A) TERMINATED BY $00, (CHARAC), OR (ENDCHR)
	jsr	POINT		; SET TXTPTR TO POINT AT STRING
	jsr	PUTSTR		; STORE STRING IN VARIABLE
	jmp	INPUT_MORE
; ----------------------------------------------------------------------------
@5:	pha
	lda	INPUTBUFFER	; ANYTHING IN BUFFER?
	beq	INPFIN		; NO, SEE IF READ OR INPUT

; ----------------------------------------------------------------------------
INPUT_DATA:
	pla			; "READ"
	jsr	FIN		; GET FP NUMBER AT TXTPTR
	lda	VALTYP+1
	jsr	LET2		; STORE RESULT IN VARIABLE

; ----------------------------------------------------------------------------
INPUT_MORE:
	jsr	CHRGOT
	beq	@1		; END OF LINE OR COLON
	cmp	#','		; COMMA IN INPUT?
	beq	@1		; YES
	jmp	INPUTERR	; NOTHING ELSE WILL DO
@1:	lda	TXTPTR		; SAVE POSITION IN INPUT BUFFER
	ldy	TXTPTR+1
	sta	INPTR
	sty	INPTR+1
	lda	TXPSV		; RESTORE PROGRAM POINTER
	ldy	TXPSV+1
	sta	TXTPTR
	sty	TXTPTR+1
	jsr	CHRGOT		; NEXT CHAR FROM PROGRAM
	beq	INPDONE		; END OF STATEMENT
	jsr	CHKCOM		; BETTER BE A COMMA THEN
	jmp	PROCESS_INPUT_ITEM

; ----------------------------------------------------------------------------
INPFIN:	lda	INPUTFLG	; "INPUT" OR "READ"
	bne	INPUT_DATA	; "READ"
	jmp	INPERR

; ----------------------------------------------------------------------------
FINDATA:
	jsr	DATAN		; GET OFFSET TO NEXT COLON OR EOL
	iny			; TO FIRST CHAR OF NEXT LINE
	tax			; WHICH:  EOL OR COLON?
	bne	@1		; COLON
	ldx	#ERR_NODATA	; EOL: MIGHT BE OUT OF DATA
	iny			; CHECK HI-BYTE OF FORWARD PNTR
	lda	(TXTPTR),y	; END OF PROGRAM?
	beq	GERR		; YES, WE ARE OUT OF DATA
	iny			; PICK UP THE LINE #
	lda	(TXTPTR),y
	sta	DATLIN
	iny
	lda	(TXTPTR),y
	iny			; POINT AT FIRST TEXT CHAR IN LINE
	sta	DATLIN+1
@1:	lda	(TXTPTR),y	; GET 1ST TOKEN OF STATEMENT
	tax			; SAVE TOKEN IN X-REG
	jsr	ADDON		; ADD (Y) TO TXTPTR
	cpx	#TOKEN_DATA	; DID WE FIND A "DATA" STATEMENT?
	bne	FINDATA		; NOT YET
	jmp	INSTART		; YES, READ IT
; ---NO MORE INPUT REQUESTED------
INPDONE:
	lda	INPTR		; GET POINTER IN CASE IT WAS "READ"
	ldy	INPTR+1
	ldx	INPUTFLG	; "READ" OR "INPUT"?
	bpl	@1		; "INPUT"
	jmp	SETDA		; "DATA", SO STORE (Y,X) AT DATPTR
@1:	ldy	#0		; "INPUT":  ANY MORE CHARS ON LINE?
	lda	(INPTR),y
	beq	@2		; NO, ALL IS WELL
	lda	#<ERREXTRA	; YES, ERROR
	ldy	#>ERREXTRA	; "EXTRA IGNORED"
	jmp	STROUT
@2:	rts

; ----------------------------------------------------------------------------
ERREXTRA:
	.byte	"?EXTRA IGNORED",$0D,$00
ERRREENTRY:
	.byte	"?REENTER",$0D,$00


; ----------------------------------------------------------------------------
; "NEXT" STATEMENT
; ----------------------------------------------------------------------------
NEXT:	bne	NEXT1		; VARIABLE AFTER "NEXT"
	ldy	#0		; FLAG BY SETTING FORPNT+1 = 0
	beq	NEXT2		; ...ALWAYS
; ----------------------------------------------------------------------------
NEXT1:	jsr	PTRGET		; GET PNTR TO VARIABLE IN (Y,A)
NEXT2:	sta	FORPNT
	sty	FORPNT+1
	jsr	GTFORPNT	; FIND FOR-FRAME FOR THIS VARIABLE
	beq	NEXT3		; FOUND IT
	ldx	#ERR_NOFOR	; NOT THERE, ABORT
GERR:	beq	JERROR		; ...ALWAYS
NEXT3:	txs			; SET STACK PTR TO POINT TO THIS FRAME,
	inx			;    WHICH TRIMS OFF ANY INNER LOOPS
	inx
	inx
	inx
	txa			; LOW BYTE OF ADRS OF STEP VALUE
	inx
	inx
	inx
	inx
	inx
	inx
	stx	DEST		; LOW BYTE ADRS OF FOR VAR VALUE
	ldy	#>STACK		; (Y,A) IS ADDRESS OF STEP VALUE
	jsr	LOAD_FAC_FROM_YA	; STEP TO FAC
	tsx
	lda	STACK+9,x
	sta	FACSIGN
	lda	FORPNT
	ldy	FORPNT+1
	jsr	FADD		; ADD TO FOR VALUE
	jsr	SETFOR		; PUT NEW VALUE BACK
	ldy	#>STACK		; (Y,A) IS ADDRESS OF END VALUE
	jsr	FCOMP2		; COMPARE TO END VALUE
	tsx
	sec
	sbc	STACK+9,x	; SIGN OF STEP
	beq	@2		; BRANCH IF FOR COMPLETE
	lda	STACK+15,x	; OTHERWISE SET UP
	sta	CURLIN		; FOR LINE #
	lda	STACK+16,x
	sta	CURLIN+1
	lda	STACK+18,x	; AND SET TXTPTR TO JUST
	sta	TXTPTR		; AFTER FOR STATEMENT
	lda	STACK+17,x
	sta	TXTPTR+1
@1:	jmp	NEWSTT
@2:	txa			; POP OFF FOR-FRAME, LOOP IS DONE
	adc	#17		; CARRY IS SET, SO ADDS 18
	tax
	txs
	jsr	CHRGOT		; CHAR AFTER VARIABLE
	cmp	#','		; ANOTHER VARIABLE IN NEXT?
	bne	@1		; NO, GO TO NEXT STATEMENT
	jsr	CHRGET		; YES, PRIME FOR NEXT VARIABLE
	jsr	NEXT1		; (DOES NOT RETURN)


; ----------------------------------------------------------------------------
; EVALUATE EXPRESSION, MAKE SURE IT IS NUMERIC
; ----------------------------------------------------------------------------
FRMNUM:	jsr	FRMEVL


; ----------------------------------------------------------------------------
; MAKE SURE (FAC) IS NUMERIC
; ----------------------------------------------------------------------------
CHKNUM:	clc
	.byte	$24		; DUMMY FOR SKIP


; ----------------------------------------------------------------------------
; MAKE SURE (FAC) IS STRING
; ----------------------------------------------------------------------------
CHKSTR:	sec


; ----------------------------------------------------------------------------
; MAKE SURE (FAC) IS CORRECT TYPE
; IF C=0, TYPE MUST BE NUMERIC
; IF C=1, TYPE MUST BE STRING
; ----------------------------------------------------------------------------
CHKVAL:	bit     VALTYP		; $00 IF NUMERIC, $FF IF STRING
	bmi     @2		; TYPE IS STRING
	bcs     @3		; NOT STRING, BUT WE NEED STRING
@1:	rts			; TYPE IS CORRECT
@2:	bcs     @1		; IS STRING AND WE WANTED STRING
@3:	ldx     #ERR_BADTYPE	; TYPE MISMATCH
JERROR: jmp     ERROR


; ----------------------------------------------------------------------------
; EVALUATE THE EXPRESSION AT TXTPTR, LEAVING THE
; RESULT IN FAC.  WORKS FOR BOTH STRING AND NUMERIC
; EXPRESSIONS.
; ----------------------------------------------------------------------------
FRMEVL:	ldx	TXTPTR		; DECREMENT TXTPTR
	bne	@1
	dec	TXTPTR+1
@1:	dec	TXTPTR
	ldx	#0		; START WITH PRECEDENCE = 0
	.byte	$24		; TRICK TO SKIP FOLLOWING "PHA"
; ----------------------------------------------------------------------------
FRMEVL1:
	pha			; PUSH RELOPS FLAGS
	txa
	pha			; SAVE LAST PRECEDENCE
	lda	#1
	jsr	CHKMEM		; CHECK IF ENOUGH ROOM ON STACK
	jsr	FRM_ELEMENT	; GET AN ELEMENT
	lda	#0
	sta	CPRTYP		; CLEAR COMPARISON OPERATOR FLAGS
; ----------------------------------------------------------------------------
FRMEVL2:
	jsr	CHRGOT		; CHECK FOR RELATIONAL OPERATORS
@1:	sec			; > IS $AE, = IS $AF, < IS $B0
	sbc	#TOKEN_GREATER	; > IS 0, = IS 1, < IS 2
	bcc	@2		; NOT RELATIONAL OPERATOR
	cmp	#3
	bcs	@2		; NOT RELATIONAL OPERATOR
	cmp	#1		; SET CARRY IF "=" OR "<"
	rol	a		; NOW > IS 0, = IS 3, < IS 5
	eor	#1		; NOW > IS 1, = IS 2, < IS 4
	eor	CPRTYP		; SET BITS OF CPRTYP:  00000<=>
	cmp	CPRTYP		; CHECK FOR ILLEGAL COMBINATIONS
	bcc	SNTXERR		; IF LESS THAN, A RELOP WAS REPEATED
	sta	CPRTYP
	jsr	CHRGET		; ANOTHER OPERATOR?
	jmp	@1		; CHECK FOR <,=,> AGAIN
; ----------------------------------------------------------------------------
@2:	ldx	CPRTYP		; DID WE FIND A RELATIONAL OPERATOR?
	bne	FRM_RELATIONAL	; YES
	bcs	NOTMATH		; NO, AND NEXT TOKEN IS > $D1
	adc	#TOKEN_GREATER-TOKEN_PLUS	; NO, AND NEXT TOKEN < $CF
	bcc	NOTMATH		; IF NEXT TOKEN < "+"
	adc	VALTYP		; + AND LAST RESULT A STRING?
	bne	@3		; BRANCH IF NOT
	jmp	CAT		; CONCATENATE IF SO.
; ----------------------------------------------------------------------------
@3:	adc	#$FF		; +-*/ IS 0123
	sta	INDEX
	asl	a		; MULTIPLY BY 3
	adc	INDEX		; +-*/ IS 0,3,6,9
	tay

; ----------------------------------------------------------------------------
FRM_PRECEDENCE_TEST:
	pla			; GET LAST PRECEDENCE
	cmp	MATHTBL,y
	bcs	FRM_PERFORM1	; DO NOW IF HIGHER PRECEDENCE
	jsr	CHKNUM		; WAS LAST RESULT A #?
NXOP:	pha			; YES, SAVE PRECEDENCE ON STACK
SAVOP:	jsr	FRM_RECURSE	; SAVE REST, CALL FRMEVL RECURSIVELY
	pla
	ldy	LASTOP
	bpl	PREFNC
	tax
	beq	GOEX		; EXIT IF NO MATH IN EXPRESSION
	bne	FRM_PERFORM2	; ...ALWAYS


; ----------------------------------------------------------------------------
; FOUND ONE OR MORE RELATIONAL OPERATORS <,=,>
; ----------------------------------------------------------------------------
FRM_RELATIONAL:
	lsr	VALTYP		; (VALTYP) = 0 (NUMERIC), = $FF (STRING)
	txa			; SET CPRTYP TO 0000<=>C
	rol	a		; WHERE C=0 IF #, C=1 IF STRING
	ldx	TXTPTR		; BACK UP TXTPTR
	bne	@1
	dec	TXTPTR+1
@1:	dec	TXTPTR
	ldy	#M_REL-MATHTBL	; POINT AT RELOPS ENTRY
	sta	CPRTYP
	bne	FRM_PRECEDENCE_TEST	; ...ALWAYS
; ----------------------------------------------------------------------------
PREFNC:	cmp	MATHTBL,y
	bcs	FRM_PERFORM2	; DO NOW IF HIGHER PRECEDENCE
	bcc	NXOP		; ...ALWAYS


; ----------------------------------------------------------------------------
; STACK THIS OPERATION AND CALL FRMEVL FOR
; ANOTHER ONE
; ----------------------------------------------------------------------------
FRM_RECURSE:
	lda	MATHTBL+2,y
	pha			; PUSH ADDRESS OF OPERATION PERFORMER
	lda	MATHTBL+1,y
	pha
	jsr	FRM_STACK1	; STACK FAC.SIGN AND FAC
	lda	CPRTYP		; A=RELOP FLAGS, X=PRECEDENCE BYTE
	jmp	FRMEVL1		; RECURSIVELY CALL FRMEVL
; ----------------------------------------------------------------------------
SNTXERR:
	jmp	SYNERR


; ----------------------------------------------------------------------------
; STACK (FAC)
; THREE ENTRY POINTS:
; 	1, FROM FRMEVL
;	2, FROM "STEP"
;	3, FROM "FOR"
; ----------------------------------------------------------------------------
FRM_STACK1:
	lda	FACSIGN		; GET FAC.SIGN TO PUSH IT
	ldx	MATHTBL,y	; PRECEDENCE BYTE FROM MATHTBL


; ----------------------------------------------------------------------------
; ENTER HERE FROM "STEP", TO PUSH STEP SIGN AND VALUE
; ----------------------------------------------------------------------------
FRM_STACK2:
	tay			; FAC.SIGN OR SGN(STEP VALUE)
	pla			; PULL RETURN ADDRESS AND ADD 1
	sta	INDEX		; <<< ASSUMES NOT ON PAGE BOUNDARY! >>>
	inc	INDEX		; PLACE BUMPED RETURN ADDRESS IN
	pla			; 	INDEX,INDEX+1
	sta	INDEX+1
	tya			; FAC.SIGN OR SGN(STEP VALUE)
	pha			; PUSH FAC.SIGN OR SGN(STEP VALUE)


; ----------------------------------------------------------------------------
; ENTER HERE FROM "FOR", WITH (INDEX) = STEP,
; TO PUSH INITIAL VALUE OF "FOR" VARIABLE
; ----------------------------------------------------------------------------
FRM_STACK3:
	jsr	ROUND_FAC	; ROUND TO 32 BITS
	lda	FAC+4		; PUSH (FAC)
	pha
	lda	FAC+3
	pha
	lda	FAC+2
	pha
	lda	FAC+1
	pha
	lda	FAC
	pha
	jmp	(INDEX)		; DO RTS FUNNY WAY


; ----------------------------------------------------------------------------
NOTMATH:
	ldy	#$FF		; SET UP TO EXIT ROUTINE
	pla
GOEX:	beq	EXIT		; EXIT IF NO MATH TO DO


; ----------------------------------------------------------------------------
; PERFORM STACKED OPERATION
;
; (A) = PRECEDENCE BYTE
; STACK:  1 -- CPRMASK
;	5 -- (ARG)
;	2 -- ADDR OF PERFORMER
; ----------------------------------------------------------------------------
FRM_PERFORM1:
	cmp	#PREL		; WAS IT RELATIONAL OPERATOR?
	beq	@1		; YES, ALLOW STRING COMPARE
	jsr	CHKNUM		; MUST BE NUMERIC VALUE
@1:	sty	LASTOP
FRM_PERFORM2:
	pla			; GET 0000<=>C FROM STACK
	lsr	a		; SHIFT TO 00000<=> FORM
	sta	CPRMASK		; 00000<=>
	pla
	sta	ARG		; GET FLOATING POINT VALUE OFF STACK,
	pla			; AND PUT IT IN ARG
	sta	ARG+1
	pla
	sta	ARG+2
	pla
	sta	ARG+3
	pla
	sta	ARG+4
	pla
	sta	ARG+5
	eor	FACSIGN		; SAVE EOR OF SIGNS OF THE OPERANDS,
	sta	SGNCPR		; IN CASE OF MULTIPLY OR DIVIDE
EXIT:	lda	FAC		; FAC EXPONENT IN A-REG / STATUS .EQ. IF (FAC)=0
	rts			; RTS GOES TO PERFORM OPERATION


; ----------------------------------------------------------------------------
; GET ELEMENT IN EXPRESSION
;
; GET VALUE OF VARIABLE OR NUMBER AT TXTPNT, OR POINT
; TO STRING DESCRIPTOR IF A STRING, AND PUT IN FAC.
; ----------------------------------------------------------------------------
FRM_ELEMENT:
	lda	#0		; ASSUME NUMERIC
	sta	VALTYP
@1:	jsr	CHRGET
	bcs	@3		; NOT A DIGIT
@2:	jmp	FIN		; NUMERIC CONSTANT
@3:	jsr	ISLETC		; VARIABLE NAME?
	bcs	FRM_VARIABLE	; YES
	cmp	#'.'		; DECIMAL POINT
	beq	@2		; YES, NUMERIC CONSTANT
	cmp	#TOKEN_MINUS	; UNARY MINUS?
	beq	MIN		; YES
	cmp	#TOKEN_PLUS	; UNARY PLUS
	beq	@1		; YES
	cmp	#'"'		; STRING CONSTANT?
	bne	NOT_		; NO


; ----------------------------------------------------------------------------
; STRING CONSTANT ELEMENT
;
; SET Y,A = (TXTPTR)+CARRY
; ----------------------------------------------------------------------------
STRTXT:	lda	TXTPTR		; ADD (CARRY) TO GET ADDRESS OF 1ST CHAR
	ldy	TXTPTR+1	;    OF STRING IN Y,A
	adc	#0
	bcc	@1
	iny
@1:	jsr	STRLIT		; BUILD DESCRIPTOR TO STRING / GET ADDRESS OF DESCRIPTOR IN FAC
	jmp	POINT		; POINT TXTPTR AFTER TRAILING QUOTE


; ----------------------------------------------------------------------------
; "NOT" FUNCTION
; IF FAC=0, RETURN FAC=1
; IF FAC<>0, RETURN FAC=0
; ----------------------------------------------------------------------------
NOT_:	cmp	#TOKEN_NOT
	bne	SGN_		; NOT "NOT", TRY "SGN"
	ldy	#M_EQU-MATHTBL	; POINT AT = COMPARISON
	bne	EQUL		; ...ALWAYS


; ----------------------------------------------------------------------------
; COMPARISON FOR EQUALITY (= OPERATOR)
; ALSO USED TO EVALUATE "NOT" FUNCTION
; ----------------------------------------------------------------------------
EQUOP:	lda	FAC		; SET "TRUE" IF (FAC) = ZERO
	bne	@1		; FALSE
	ldy	#1		; TRUE
	.byte	$2C		; TRICK TO SKIP NEXT 2 BYTES
@1:	ldy	#0		; FALSE
	jmp	SNGFLT
; ----------------------------------------------------------------------------
SGN_:	cmp	#TOKEN_SGN
	bcc	PARCHK
	jmp	UNARY


; ----------------------------------------------------------------------------
; EVALUATE "(EXPRESSION)"
; ----------------------------------------------------------------------------
PARCHK:	jsr	CHKOPN		; IS THERE A '(' AT TXTPTR?
	jsr	FRMEVL		; YES, EVALUATE EXPRESSION
; ----------------------------------------------------------------------------
CHKCLS:	lda	#')'		; CHECK FOR ')'
	.byte	$2C		; TRICK
; ----------------------------------------------------------------------------
CHKOPN:	lda	#'('
	.byte   $2C		; TRICK
; ----------------------------------------------------------------------------
CHKCOM:	lda	#','		; COMMA AT TXTPTR?


; ----------------------------------------------------------------------------
; UNLESS CHAR AT TXTPTR = (A), SYNTAX ERROR
; ----------------------------------------------------------------------------
SYNCHR:	ldy	#0
	cmp	(TXTPTR),y
	bne	SYNERR
	jmp	CHRGET		; MATCH, GET NEXT CHAR & RETURN
; ----------------------------------------------------------------------------
SYNERR:	ldx	#ERR_SYNTAX
	jmp	ERROR
; ----------------------------------------------------------------------------
MIN:	ldy     #M_NEG-MATHTBL	; POINT AT UNARY MINUS
EQUL:	pla
	pla
	jmp	SAVOP
; ----------------------------------------------------------------------------
FRM_VARIABLE:
	jsr	PTRGET
FRM_VARIABLE_CALL	= *-1	; SO PTRGET CAN TELL WE CALLED
	sta	VPNT		; ADDRESS OF VARIABLE
	sty	VPNT+1
	ldx	VALTYP		; NUMERIC OR STRING?
	beq	@1		; NUMERIC
	ldx	#0		; STRING
	stx	STRNG1+1
	rts
@1:	ldx	VALTYP+1	; NUMERIC, WHICH TYPE?
	bpl	@2		; FLOATING POINT
	ldy	#0		; INTEGER
	lda	(VPNT),y
	tax			; GET VALUE IN A,Y
	iny
	lda	(VPNT),y
	tay
	txa
	jmp	GIVAYF		; CONVERT A,Y TO FLOATING POINT
@2:	jmp	LOAD_FAC_FROM_YA


; ----------------------------------------------------------------------------
UNARY:	asl	a		; DOUBLE TOKEN TO GET INDEX
	pha
	tax
	jsr	CHRGET
	cpx	#<(TOKEN_LEFTSTR*2-1)	; LEFT$, RIGHT$, AND MID$
	bcc	@1		; NOT ONE OF THE STRING FUNCTIONS
	jsr	CHKOPN		; STRING FUNCTION, NEED "("
	jsr	FRMEVL		; EVALUATE EXPRESSION FOR STRING
	jsr	CHKCOM		; REQUIRE A COMMA
	jsr	CHKSTR		; MAKE SURE EXPRESSION IS A STRING
	pla
	tax			; RETRIEVE ROUTINE POINTER
	lda	VPNT+1		; STACK ADDRESS OF STRING
	pha
	lda	VPNT
	pha
	txa
	pha			; STACK DOUBLED TOKEN
	jsr	GETBYT		; CONVERT NEXT EXPRESSION TO BYTE IN X-REG
	pla			; GET DOUBLED TOKEN OFF STACK
	tay			; USE AS INDEX TO BRANCH
	txa			; VALUE OF SECOND PARAMETER
	pha			; PUSH 2ND PARAM
	jmp	@2		; JOIN UNARY FUNCTIONS
@1:	jsr	PARCHK		; REQUIRE "(EXPRESSION)"
	pla
	tay			; INDEX INTO FUNCTION ADDRESS TABLE
@2:	lda	UNFNC-TOKEN_SGN-TOKEN_SGN+$100,y
	sta	JMPADRS+1	; PREPARE TO JSR TO ADDRESS
	lda	UNFNC-TOKEN_SGN-TOKEN_SGN+$101,y
	sta	JMPADRS+2
	jsr	JMPADRS		; DOES NOT RETURN FOR CHR$, LEFT$, RIGHT$, OR MID$
	jmp	CHKNUM		; REQUIRE NUMERIC RESULT
; ----------------------------------------------------------------------------
OR:	lda	ARG		; "OR" OPERATOR
	ora	FAC		; IF RESULT NONZERO, IT IS TRUE
	bne	TRUE
; ----------------------------------------------------------------------------
TAND:	lda	ARG		; "AND" OPERATOR
	beq	FALSE		; IF EITHER IS ZERO, RESULT IS FALSE
	lda	FAC
	bne	TRUE
; ----------------------------------------------------------------------------
FALSE:	ldy	#0		; RETURN FAC=0
	.byte	$2C		; TRICK
; ----------------------------------------------------------------------------
TRUE:	ldy	#1		; RETURN FAC=1
	jmp	SNGFLT


; ----------------------------------------------------------------------------
; PERFORM RELATIONAL OPERATIONS
; ----------------------------------------------------------------------------
RELOPS:	jsr	CHKVAL		; MAKE SURE FAC IS CORRECT TYPE
	bcs	STRCMP		; TYPE MATCHES, BRANCH IF STRINGS
	lda	ARGSIGN		; NUMERIC COMPARISON
	ora	#$7F		; RE-PACK VALUE IN ARG FOR FCOMP
	and	ARG+1
	sta	ARG+1
	lda	#<ARG
	ldy	#>ARG
	jsr	FCOMP		; RETURN A-REG = -1,0,1
	tax			; AS ARG <,=,> FAC
	jmp	NUMCMP


; ----------------------------------------------------------------------------
; STRING COMPARISON
; ----------------------------------------------------------------------------
STRCMP:	lda	#0		; SET RESULT TYPE TO NUMERIC
	sta	VALTYP
	dec	CPRTYP		; MAKE CPRTYP 0000<=>0
	jsr	FREFAC
	sta	FAC		; STRING LENGTH
	stx	FAC+1
	sty	FAC+2
	lda	ARG+3
	ldy	ARG+4
	jsr	FRETMP
	stx	ARG+3
	sty	ARG+4
	tax			; LEN (ARG) STRING
	sec
	sbc	FAC		; SET X TO SMALLER LEN
	beq	@1
	lda	#1
	bcc	@1
	ldx	FAC
	lda	#$FF
@1:	sta	FACSIGN		; FLAG WHICH SHORTER
	ldy	#$FF
	inx
STRCMP1:
	iny
	dex
	bne	STRCMP2		; MORE CHARS IN BOTH STRINGS
	ldx	FACSIGN		; IF = SO FAR, DECIDE BY LENGTH
; ----------------------------------------------------------------------------
NUMCMP:	bmi	CMPDONE
	clc
	bcc	CMPDONE		; ...ALWAYS
; ----------------------------------------------------------------------------
STRCMP2:
	lda	(ARG+3),y
	cmp	(FAC+1),y
	beq	STRCMP1		; SAME, KEEP COMPARING
	ldx	#$FF		; IN CASE ARG GREATER
	bcs	CMPDONE		; IT IS
	ldx	#1		; FAC GREATER
; ----------------------------------------------------------------------------
CMPDONE:
	inx			; CONVERT FF,0,1 TO 0,1,2
	txa
	rol	a		; AND TO 0,2,4 IF C=0, ELSE 1,2,5
	and	CPRMASK		; 00000<=>
	beq	@1		; IF NO MATCH: FALSE
	lda	#1		; AT LEAST ONE MATCH: TRUE
@1:	jmp	FLOAT


; ----------------------------------------------------------------------------
; "DIM" STATEMENT
; ----------------------------------------------------------------------------
NXDIM:	jsr	CHKCOM		; SEPARATED BY COMMAS
DIM:	tax			; NON-ZERO, FLAGS PTRGET DIM CALLED
	jsr	PTRGET2		; ALLOCATE THE ARRAY
	jsr	CHRGOT		; NEXT CHAR
	bne	NXDIM		; NOT END OF STATEMENT
	rts


; ----------------------------------------------------------------------------
; PTRGET -- GENERAL VARIABLE SCAN
;
; SCANS VARIABLE NAME AT TXTPTR, AND SEARCHES THE
; VARTAB AND ARYTAB FOR THE NAME.
; IF NOT FOUND, CREATE VARIABLE OF APPROPRIATE TYPE.
; RETURN WITH ADDRESS IN VARPNT AND Y,A
;
; ACTUAL ACTIVITY CONTROLLED SOMEWHAT BY TWO FLAGS:
;	DIMFLG -- NONZERO IF CALLED FROM "DIM"
;		ELSE = 0
;
;	SUBFLG -- = $00
;		= $40 IF CALLED FROM "GETARYPT"
; ----------------------------------------------------------------------------
PTRGET:	ldx	#0
	jsr	CHRGOT		; GET FIRST CHAR OF VARIABLE NAME
; ----------------------------------------------------------------------------
PTRGET2:
	stx	DIMFLG		; X IS NONZERO IF FROM DIM
; ----------------------------------------------------------------------------
PTRGET3:
	sta	VARNAM
	jsr	CHRGOT
	jsr	ISLETC		; IS IT A LETTER?
	bcs	NAMOK		; YES, OKAY SO FAR
BADNAM:	jmp	SYNERR		; NO, SYNTAX ERROR
NAMOK:	ldx	#0
	stx	VALTYP
	stx	VALTYP+1
; ----------------------------------------------------------------------------
PTRGET4:
	jsr	CHRGET		; SECOND CHAR OF VARIABLE NAME
	bcc	@1		; NUMERIC
	jsr	ISLETC		; LETTER?
	bcc	@3		; NO, END OF NAME
@1:	tax			; SAVE SECOND CHAR OF NAME IN X
@2:	jsr	CHRGET		; SCAN TO END OF VARIABLE NAME
	bcc	@2		; NUMERIC
	jsr	ISLETC
	bcs	@2		; ALPHA
@3:	cmp	#'$'		; STRING?
	bne	@4		; NO
	lda	#$FF
	sta	VALTYP
	bne	@5		; ...ALWAYS
@4:	cmp	#'%'		; INTEGER?
	bne	@6		; NO
	lda	SUBFLG		; YES; INTEGER VARIABLE ALLOWED?
	bmi	BADNAM		; NO, SYNTAX ERROR
	lda	#$80		; YES
	sta	VALTYP+1	; FLAG INTEGER MODE
	ora	VARNAM
	sta	VARNAM		; SET SIGN BIT ON VARNAME
@5:	txa			; SECOND CHAR OF NAME
	ora	#$80		; SET SIGN
	tax
	jsr	CHRGET		; GET TERMINATING CHAR
@6:	stx	VARNAM+1	; STORE SECOND CHAR OF NAME
	sec
	ora	SUBFLG		; $00 OR $40 IF SUBSCRIPTS OK, ELSE $80
	sbc	#'('		; IF SUBFLG=$00 AND CHAR="("...
	bne	@8		; NOPE
@7:	jmp	ARRAY		; YES
@8:	bit	SUBFLG		; CHECK TOP TWO BITS OF SUBFLG
	bmi	@9		; $80
	bvs	@7		; $40, CALLED FROM GETARYPT
@9:	lda	#0		; CLEAR SUBFLG
	sta	SUBFLG
	lda	VARTAB		; START LOWTR AT SIMPLE VARIABLE TABLE
	ldx	VARTAB+1
	ldy	#0
@10:	stx	LOWTR+1
@11:	sta	LOWTR
	cpx	ARYTAB+1	; END OF SIMPLE VARIABLES?
	bne	@12		; NO, GO ON
	cmp	ARYTAB		; YES; END OF ARRAYS?
	beq	NAMENOTFOUND	; YES, MAKE ONE
@12:	lda	VARNAM		; SAME FIRST LETTER?
	cmp	(LOWTR),y
	bne	@13		; NOT SAME FIRST LETTER
	lda	VARNAM+1	; SAME SECOND LETTER?
	iny
	cmp	(LOWTR),y
	beq	SET_VARPNT_AND_YA	; YES, SAME VARIABLE NAME
	dey			; NO, BUMP TO NEXT NAME
@13:	clc
	lda	LOWTR
	adc	#7
	bcc	@11
	inx
	bne	@10		; ...ALWAYS


; ----------------------------------------------------------------------------
; CHECK IF (A) IS ASCII LETTER A-Z
;
; RETURN CARRY = 1 IF A-Z
;	= 0 IF NOT
; ----------------------------------------------------------------------------
ISLETC:	cmp	#'Z'+1		; COMPARE HI END
	bcs	@1		; ABOVE A-Z
	cmp	#'A'		; COMPARE LO END
	rts			; C=0 IF LO, C=1 IF A-Z
@1:	clc			; C=0 IF HI
	rts


; ----------------------------------------------------------------------------
; VARIABLE NOT FOUND, SO MAKE ONE
; ----------------------------------------------------------------------------
NAMENOTFOUND:
	pla			; LOOK AT RETURN ADDRESS ON STACK TO
	pha			; SEE IF CALLED FROM FRM.VARIABLE
	cmp	#<FRM_VARIABLE_CALL
	bne	MAKENEWVARIABLE	; NO
	tsx
	lda	STACK+2,x
	cmp	#>FRM_VARIABLE_CALL
	bne	MAKENEWVARIABLE	; NO
	lda	#<C_ZERO	; YES, CALLED FROM FRM.VARIABLE
	ldy	#>C_ZERO	; POINT TO A CONSTANT ZERO
	rts			; NEW VARIABLE USED IN EXPRESSION = 0

; ----------------------------------------------------------------------------
C_ZERO:	.word	$0000		; INTEGER OR REAL ZERO, OR NULL STRING


; ----------------------------------------------------------------------------
; MAKE A NEW SIMPLE VARIABLE
;
; MOVE ARRAYS UP 7 BYTES TO MAKE ROOM FOR NEW VARIABLE
; ENTER 7-BYTE VARIABLE DATA IN THE HOLE
; ----------------------------------------------------------------------------
MAKENEWVARIABLE:
	lda	ARYTAB		; SET UP CALL TO BLTU TO
	ldy	ARYTAB+1	; TO MOVE FROM ARYTAB THRU STREND-1
	sta	LOWTR		; 7 BYTES HIGHER
	sty	LOWTR+1
	lda	STREND
	ldy	STREND+1
	sta	HIGHTR
	sty	HIGHTR+1
	clc
	adc	#7
	bcc	@1
	iny
@1:	sta	ARYPNT
	sty	ARYPNT+1
	jsr	BLTU		; MOVE ARRAY BLOCK UP
	lda	ARYPNT		; STORE NEW START OF ARRAYS
	ldy	ARYPNT+1
	iny
	sta	ARYTAB
	sty	ARYTAB+1
	ldy	#0
	lda	VARNAM		; FIRST CHAR OF NAME
	sta	(LOWTR),y
	iny
	lda	VARNAM+1	; SECOND CHAR OF NAME
	sta	(LOWTR),y
	lda	#0		; SET FIVE-BYTE VALUE TO 0
	iny
	sta	(LOWTR),y
	iny
	sta	(LOWTR),y
	iny
	sta	(LOWTR),y
	iny
	sta	(LOWTR),y
	iny
	sta	(LOWTR),y


; ----------------------------------------------------------------------------
; PUT ADDRESS OF VALUE OF VARIABLE IN VARPNT AND Y,A
; ----------------------------------------------------------------------------
SET_VARPNT_AND_YA:
	lda	LOWTR		; LOWTR POINTS AT NAME OF VARIABLE,
	clc			; SO ADD 2 TO GET TO VALUE
	adc	#2
	ldy	LOWTR+1
	bcc	@1
	iny
@1:	sta	VARPNT		; ADDRESS IN VARPNT AND Y,A
	sty	VARPNT+1
	rts


; ----------------------------------------------------------------------------
; COMPUTE ADDRESS OF FIRST VALUE IN ARRAY
; ARYPNT = (LOWTR) + #DIMS*2 + 5
; ----------------------------------------------------------------------------
GETARY:	lda	NUMDIM		; GET # OF DIMENSIONS
	asl	a		; #DIMS*2 (SIZE OF EACH DIM IN 2 BYTES)
	adc	#5		; + 5 (2 FOR NAME, 2 FOR OFFSET TO NEXT ARRAY, AND 1 FOR #DIMS
	adc	LOWTR		; ADDRESS OF TH IS ARRAY IN ARYTAB
	ldy	LOWTR+1
	bcc	@1
	iny
@1:	sta	ARYPNT		; ADDRESS OF FIRST VALUE IN ARRAY
	sty	ARYPNT+1
	rts


; ----------------------------------------------------------------------------
NEG32768:
	.byte   $90,$80,$00,$00,$00	; -32768 IN FLOATING POINT
; ----------------------------------------------------------------------------


; ----------------------------------------------------------------------------
; EVALUATE NUMERIC FORMULA AT TXTPTR
; CONVERTING RESULT TO INTEGER 0 <= X <= 32767
; IN FAC+3,4
; ----------------------------------------------------------------------------
MAKINT:	jsr	CHRGET
	jsr	FRMNUM


; ----------------------------------------------------------------------------
; CONVERT FAC TO INTEGER
; MUST BE POSITIVE AND LESS THAN 32768
; ----------------------------------------------------------------------------
MKINT:	lda	FACSIGN		; ERROR IF -
	bmi	MI1


; ----------------------------------------------------------------------------
; CONVERT FAC TO INTEGER
; MUST BE -32767 <= FAC <= 32767
; ----------------------------------------------------------------------------
AYINT:	lda	FAC		; EXPONENT OF VALUE IN FAC
	cmp	#$90		; ABS(VALUE) < 32768?
	bcc	MI2		; YES, OK FOR INTEGER
	lda	#<NEG32768	; NO
	ldy	#>NEG32768
	jsr	FCOMP
; ----------------------------------------------------------------------------
MI1:	bne	IQERR		; ILLEGAL QUANTITY
MI2:	jmp	QINT		; CONVERT TO INTEGER


; ----------------------------------------------------------------------------
; LOCATE ARRAY ELEMENT OR CREATE AN ARRAY
; ----------------------------------------------------------------------------
ARRAY:  lda	SUBFLG		; SUBSCRIPTS GIVEN?
	bne	@2		; NO
; ----------------------------------------------------------------------------
; PARSE THE SUBSCRIPT LIST
; ----------------------------------------------------------------------------
	lda	DIMFLG		; YES
	ora	VALTYP+1	; SET HIGH BIT IF %
	pha			; SAVE VALTYP AND DIMFLG ON STACK
	lda	VALTYP
	pha
	ldy	#0		; COUNT # DIMENSIONS IN Y-REG
@1:	tya			; SAVE #DIMS ON STACK
	pha
	lda	VARNAM+1	; SAVE VARIABLE NAME ON STACK
	pha
	lda	VARNAM
	pha
	jsr	MAKINT		; EVALUATE SUBSCRIPT AS INTEGER
	pla			; RESTORE VARIABLE NAME
	sta	VARNAM
	pla
	sta	VARNAM+1
	pla			; RESTORE # DIMS TO Y-REG
	tay
	tsx			; COPY VALTYP AND DIMFLG ON STACK
	lda	STACK+2,x	; TO LEAVE ROOM FOR THE SUBSCRIPT
	pha
	lda	STACK+1,x
	pha
	lda	FAC+3		; GET SUBSCRIPT VALUE AND PLACE IN THE
	sta	STACK+2,x	; STACK WHERE VALTYP & DIMFLG WERE
	lda	FAC+4
	sta	STACK+1,x
	iny			; COUNT THE SUBSCRIPT
	jsr	CHRGOT		; NEXT CHAR
	cmp	#','
	beq	@1		; COMMA, PARSE ANOTHER SUBSCRIPT
	sty	NUMDIM		; NO MORE SUBSCRIPTS, SAVE #
	jsr	CHKCLS		; NOW NEED ")"
	pla			; RESTORE VALTYPE AND DIMFLG
	sta	VALTYP
	pla
	sta	VALTYP+1
	and	#$7F		; ISOLATE DIMFLG
	sta	DIMFLG
; ----------------------------------------------------------------------------
; SEARCH ARRAY TABLE FOR THIS ARRAY NAME
; ----------------------------------------------------------------------------
@2:	ldx	ARYTAB		; (A,X) = START OF ARRAY TABLE
	lda	ARYTAB+1
@3:	stx	LOWTR		; USE LOWTR FOR RUNNING POINTER
	sta	LOWTR+1
	cmp	STREND+1	; DID WE REACH THE END OF ARRAYS YET?
	bne	@4		; NO, KEEP SEARCHING
	cpx	STREND
	beq	MAKE_NEW_ARRAY	; YES, THIS IS A NEW ARRAY NAME
@4:	ldy	#0		; POINT AT 1ST CHAR OF ARRAY NAME
	lda	(LOWTR),y	; GET 1ST CHAR OF NAME
	iny			; POINT AT 2ND CHAR
	cmp	VARNAM		; 1ST CHAR SAME?
	bne	@5		; NO, MOVE TO NEXT ARRAY
	lda	VARNAM+1	; YES, TRY 2ND CHAR
	cmp	(LOWTR),y	; SAME?
	beq	USE_OLD_ARRAY	; YES, ARRAY FOUND
@5:	iny			; POINT AT OFFSET TO NEXT ARRAY
	lda	(LOWTR),y	; ADD OFFSET TO RUNNING POINTER
	clc
	adc	LOWTR
	tax
	iny
	lda	(LOWTR),y
	adc	LOWTR+1
	bcc	@3		; ...ALWAYS


; ----------------------------------------------------------------------------
; ERROR:  BAD SUBSCRIPTS
; ----------------------------------------------------------------------------
SUBERR:	ldx	#ERR_BADSUBS
	.byte	$2C		; TRICK TO SKIP NEXT LINE


; ----------------------------------------------------------------------------
; ERROR:  ILLEGAL QUANTITY
; ----------------------------------------------------------------------------
IQERR:	ldx	#ERR_ILLQTY
JER:	jmp	ERROR


; ----------------------------------------------------------------------------
; FOUND THE ARRAY
; ----------------------------------------------------------------------------
USE_OLD_ARRAY:
	ldx	#ERR_REDIMD	; SET UP FOR REDIM'D ARRAY ERROR
	lda	DIMFLG		; CALLED FROM "DIM" STATEMENT?
	bne	JER		; YES, ERROR
	lda	SUBFLG		; NO, CHECK IF ANY SUBSCRIPTS
	beq	@1		; YES, NEED TO CHECK THE NUMBER
	sec			; NO, SIGNAL ARRAY FOUND
	rts
; ----------------------------------------------------------------------------
@1:	jsr	GETARY		; SET (ARYPNT) = ADDR OF FIRST ELEMENT
	lda	TKNCNTR		; COMPARE NUMBER OF DIMENSIONS
	ldy	#4
	cmp	(LOWTR),y
	bne	SUBERR		; NOT SAME, SUBSCRIPT ERROR
	jmp	FIND_ARRAY_ELEMENT


; ----------------------------------------------------------------------------
; CREATE A NEW ARRAY, UNLESS CALLED FROM GETARYPT
; ----------------------------------------------------------------------------
MAKE_NEW_ARRAY:
	lda	SUBFLG		; CALLED FROM GETARYPT?
	beq	@1		; NO
	ldx	#ERR_NODATA	; YES, GIVE "OUT OF DATA" ERROR
	jmp	ERROR
@1:	jsr	GETARY		; PUT ADDR OF 1ST ELEMENT IN ARYPNT
	jsr	REASON		; MAKE SURE ENOUGH MEMORY LEFT
	ldy	#0		; POINT Y-REG AT VARIABLE NAME SLOT
	sty	STRNG2+1	; START SIZE COMPUTATION
	ldx	#5		; ASSUME 5-BYTES PER ELEMENT
	lda	VARNAM		; STUFF VARIABLE NAME IN ARRAY
	sta	(LOWTR),y
	bpl	@2		; NOT INTEGER ARRAY
	dex			; INTEGER ARRAY, DECR. SIZE TO 4-BYTES
@2:	iny			; POINT Y-REG AT NEXT CHAR OF NAME
	lda	VARNAM+1	; REST OF ARRAY NAME
	sta	(LOWTR),y
	bpl	@3		; REAL ARRAY, STICK WITH SIZE = 5 BYTES
	dex			; INTEGER OR STRING ARRAY, ADJUST SIZE
	dex			; TO INTEGER=3, STRING=2 BYTES
@3:	stx	STRNG2		; STORE LOW-BYTE OF ARRAY ELEMENT SIZE
	lda	NUMDIM		; STORE NUMBER OF DIMENSIONS
	iny			; IN 5TH BYTE OF ARRAY
	iny
	iny
	sta	(LOWTR),y
@4:	ldx	#11		; DEFAULT DIMENSION = 11 ELEMENTS
	lda	#0		; FOR HI-BYTE OF DIMENSION IF DEFAULT
	bit	DIMFLG		; DIMENSIONED ARRAY?
	bvc	@5		; NO, USE DEFAULT VALUE
	pla			; GET SPECIFIED DIM IN A,X
	clc			; # ELEMENTS IS 1 LARGER THAN
	adc	#1		; DIMENSION VALUE
	tax
	pla
	adc	#0
@5:	iny			; ADD THIS DIMENSION TO ARRAY DESCRIPTOR
	sta	(LOWTR),y
	iny
	txa
	sta	(LOWTR),y
	jsr	MULTIPLY_SUBSCRIPT	; MULTIPLY THIS DIMENSION BY RUNNING SIZE ((LOWTR)) * (STRNG2) --> A,X
	stx	STRNG2		; STORE RUNNING SIZE IN STRNG2
	sta	STRNG2+1
	ldy	INDEX		; RETRIEVE Y SAVED BY MULTIPLY.SUBSCRIPT
	dec	NUMDIM		; COUNT DOWN # DIMS
	bne	@4		; LOOP TILL DONE
; ----------------------------------------------------------------------------
; NOW A,X HAS TOTAL # BYTES OF ARRAY ELEMENTS
; ----------------------------------------------------------------------------
	adc	ARYPNT+1	; COMPUTE ADDRESS OF END OF THIS ARRAY
	bcs	GME		; ...TOO LARGE, ERROR
	sta	ARYPNT+1
	tay
	txa
	adc	ARYPNT
	bcc	@6
	iny
	beq	GME		; ...TOO LARGE, ERROR
@6:	jsr	REASON		; MAKE SURE THERE IS ROOM UP TO Y,A
	sta	STREND		; THERE IS ROOM SO SAVE NEW END OF TABLE
	sty	STREND+1	;     AND ZERO THE ARRAY
	lda	#0
	inc	STRNG2+1	; PREPARE FOR FAST ZEROING LOOP
	ldy	STRNG2		; # BYTES MOD 256
	beq	@8		; FULL PAGE
@7:	dey			; CLEAR PAGE FULL
	sta	(ARYPNT),y
	bne	@7
@8:	dec	ARYPNT+1	; POINT TO NEXT PAGE
	dec	STRNG2+1	; COUNT THE PAGES
	bne	@7		; STILL MORE TO CLEAR
	inc	ARYPNT+1	; RECOVER LAST DEC, POINT AT 1ST ELEMENT
	sec
	lda	STREND		; COMPUTE OFFSET TO END OF ARRAYS
	sbc	LOWTR		; AND STORE IN ARRAY DESCRIPTOR
	ldy	#2
	sta	(LOWTR),y
	lda	STREND+1
	iny
	sbc	LOWTR+1
	sta	(LOWTR),y
	lda	DIMFLG		; WAS THIS CALLED FROM "DIM" STATEMENT?
	bne	RTS9		; YES, WE ARE FINISHED
	iny			; NO, NOW NEED TO FIND THE ELEMENT


; ----------------------------------------------------------------------------
; FIND SPECIFIED ARRAY ELEMENT
;
; (LOWTR),Y POINTS AT # OF DIMS IN ARRAY DESCRIPTOR
; THE SUBSCRIPTS ARE ALL ON THE STACK AS INTEGERS
; ----------------------------------------------------------------------------
FIND_ARRAY_ELEMENT:
	lda	(LOWTR),y	; GET # OF DIMENSIONS
	sta	NUMDIM
	lda	#0		; ZERO SUBSCRIPT ACCUMULATOR
	sta	STRNG2
FAE1:	sta	STRNG2+1
	iny
	pla			; PULL NEXT SUBSCRIPT FROM STACK
	tax			; SAVE IN FAC+3,4
	sta	FAC+3		; AND COMPARE WITH DIMENSIONED SIZE
	pla
	sta	FAC+4
	cmp	(LOWTR),y
	bcc	FAE2		; SUBSCRIPT NOT TOO LARGE
	bne	GSE		; SUBSCRIPT IS TOO LARGE
	iny			; CHECK LOW-BYTE OF SUBSCRIPT
	txa
	cmp	(LOWTR),y
	bcc	FAE3		; NOT TOO LARGE
; ----------------------------------------------------------------------------
GSE:	jmp	SUBERR		; BAD SUBSCRIPTS ERROR
GME:	jmp	MEMERR		; MEM FULL ERROR
; ----------------------------------------------------------------------------
FAE2:	iny			; BUMP POINTER INTO DESCRIPTOR
FAE3:	lda	STRNG2+1	; BYPASS MULTIPLICATION IF VALUE SO
	ora	STRNG2		; FAR = 0
	clc
	beq	@1		; IT IS ZERO SO FAR
	jsr	MULTIPLY_SUBSCRIPT	; NOT ZERO, SO MULTIPLY
	txa			; ADD CURRENT SUBSCRIPT
	adc	FAC+3
	tax
	tya
	ldy	INDEX		; RETRIEVE Y SAVED BY MULTIPLY.SUBSCRIPT
@1:	adc	FAC+4		; FINISH ADDING CURRENT SUBSCRIPT
	stx	STRNG2		; STORE ACCUMULATED OFFSET
	dec	NUMDIM		; LAST SUBSCRIPT YET?
	bne	FAE1		; NO, LOOP TILL DONE
	sta	STRNG2+1	; YES, NOW MULTIPLY BE ELEMENT SIZE
	ldx	#5		; START WITH SIZE = 5
	lda	VARNAM		; DETERMINE VARIABLE TYPE
	bpl	@2		; NOT INTEGER
	dex			; INTEGER, BACK DOWN SIZE TO 4 BYTES
@2:	lda	VARNAM+1	; DISCRIMINATE BETWEEN REAL AND STR
	bpl	@3		; IT IS REAL
	dex			; SIZE = 3 IF STRING, =2 IF INTEGER
	dex
@3:	stx	RESULT+2	; SET UP MULTIPLIER
	lda	#0		; HI-BYTE OF MULTIPLIER
	jsr	MULTIPLY_SUBS1	; (STRNG2) BY ELEMENT SIZE
	txa			; ADD ACCUMULATED OFFSET
	adc	ARYPNT		; TO ADDRESS OF 1ST ELEMENT
	sta	VARPNT		; TO GET ADDRESS OF SPECIFIED ELEMENT
	tya
	adc	ARYPNT+1
	sta	VARPNT+1
	tay			; RETURN WITH ADDR IN VARPNT
	lda	VARPNT		; AND IN Y,A
RTS9:	rts


; ----------------------------------------------------------------------------
; MULTIPLY (STRNG2) BY ((LOWTR),Y)
; LEAVING PRODUCT IN A,X.  (HI-BYTE ALSO IN Y.)
; USED ONLY BY ARRAY SUBSCRIPT ROUTINES
; ----------------------------------------------------------------------------
MULTIPLY_SUBSCRIPT:
	sty	INDEX		; SAVE Y-REG
	lda	(LOWTR),y	; GET MULTIPLIER
	sta	RESULT+2	; SAVE IN RESULT+2,3
	dey
	lda	(LOWTR),y
; ----------------------------------------------------------------------------
MULTIPLY_SUBS1:
	sta	RESULT+3	; LOW BYTE OF MULTIPLIER
	lda	#16		; MULTIPLY 16 BITS
	sta	INDX
	ldx	#0		; PRODUCT = 0 INITIALLY
	ldy	#0
@1:	txa			; DOUBLE PRODUCT
	asl	a		; LOW BYTE
	tax
	tya			; HIGH BYTE
	rol	a		; IF TOO LARGE, SET CARRY
	tay
	bcs	GME		; TOO LARGE, "MEM FULL ERROR"
	asl	STRNG2		; NEXT BIT OF MUTLPLICAND
	rol	STRNG2+1	;	INTO CARRY
	bcc	@2		; BIT=0, DON'T NEED TO ADD
	clc			; BIT=1, ADD INTO PARTIAL PRODUCT
	txa
	adc	RESULT+2
	tax
	tya
	adc	RESULT+3
	tay
	bcs	GME		; TOO LARGE, "MEM FULL ERROR"
@2:	dec	INDX		; 16-BITS YET?
	bne	@1		; NO, KEEP SHUFFLING
	rts			; YES, PRODUCT IN Y,X AND A,X


; ----------------------------------------------------------------------------
; "FRE" FUNCTION
;
; COLLECTS GARBAGE AND RETURNS # BYTES OF MEMORY LEFT
; ----------------------------------------------------------------------------
FRE:	lda	VALTYP		; LOOK AT VALUE OF ARGUMENT
	beq	@1		; =0 MEANS REAL, =$FF MEANS STRING
	jsr	FREFAC		; STRING, SO SET IT FREE IS TEMP
@1:	jsr	GARBAG		; COLLECT ALL THE GARBAGE IN SIGHT
	sec			; COMPUTE SPACE BETWEEN ARRAYS AND
	lda	FRETOP		; STRING TEMP AREA
	sbc	STREND
	tay
	lda	FRETOP+1
	sbc	STREND+1	; FREE SPACE IN Y,A
; FALL INTO GIVAYF TO FLOAT THE VALUE
; NOTE THAT VALUES OVER 32767 WILL RETURN AS NEGATIVE


; ----------------------------------------------------------------------------
; FLOAT THE SIGNED INTEGER IN A,Y
; ----------------------------------------------------------------------------
GIVAYF:	ldx	#0		; MARK FAC VALUE TYPE REAL
	stx	VALTYP
	sta	FAC+1		; SAVE VALUE FROM A,Y IN MANTISSA
	sty	FAC+2
	ldx	#$90		; SET EXPONENT TO 2^16
	jmp	FLOAT1		; CONVERT TO SIGNED FP


; ----------------------------------------------------------------------------
; FLOAT (Y) INTO FAC, GIVING VALUE 0-255
; ----------------------------------------------------------------------------
SNGFLT:	lda	#0		; MSB = 0
	beq	GIVAYF		; ...ALWAYS


; ----------------------------------------------------------------------------
; CHECK FOR DIRECT OR RUNNING MODE
; GIVING ERROR IF DIRECT MODE
; ----------------------------------------------------------------------------
ERRDIR:	ldx	CURLIN+1	; =$FF IF DIRECT MODE
	inx			; MAKES $FF INTO ZERO
	bne	RTS9		; RETURN IF RUNNING MODE
	ldx	#ERR_ILLDIR	; DIRECT MODE, GIVE ERROR
	jmp	ERROR


; ----------------------------------------------------------------------------
; "STR$" FUNCTION
; ----------------------------------------------------------------------------
STR:	jsr	CHKNUM		; EXPRESSION MUST BE NUMERIC
	ldy	#0		; START STRING AT STACK-1 ($00FF) SO STRLIT CAN DIFFRENTIATE STR$ CALLS
	jsr	FOUT1		; CONVERT FAC TO STRING
	pla			; POP RETURN OFF STACK
	pla
	lda	#<STACK-1	; POINT TO STACK-1
	ldy	#>STACK-1	; (WHICH=0)
	beq	STRLIT		; ...ALWAYS, CREATE DESC & MOVE STRING


; ----------------------------------------------------------------------------
; GET SPACE AND MAKE DESCRIPTOR FOR STRING WHOSE
; ADDRESS IS IN FAC+3,4 AND WHOSE LENGTH IS IN A-REG
; ----------------------------------------------------------------------------
STRINI:	ldx	FAC+3		; Y,X = STRING ADDRESS
	ldy	FAC+4
	stx	DSCPTR
	sty	DSCPTR+1


; ----------------------------------------------------------------------------
; GET SPACE AND MAKE DESCRIPTOR FOR STRING WHOSE
; ADDRESS IS IN Y,X AND WHOSE LENGTH IS IN A-REG
; ----------------------------------------------------------------------------
STRSPA:	jsr	GETSPA		; A HOLDS LENGTH
	stx	FAC+1		; SAVE DESCRIPTOR IN FAC
	sty	FAC+2		; ---FAC--- --FAC+1-- --FAC+2--
	sta	FAC		; <LENGTH>  <ADDR-LO> <ADDR-HI>
	rts


; ----------------------------------------------------------------------------
; BUILD A DESCRIPTOR FOR STRING STARTING AT Y,A
; AND TERMINATED BY $00 OR QUOTATION MARK
; RETURN WITH DESCRIPTOR IN A TEMPORARY
; AND ADDRESS OF DESCRIPTOR IN FAC+3,4
; ----------------------------------------------------------------------------
STRLIT:	ldx	#'"'		; SET UP LITERAL SCAN TO STOP ON
	stx	CHARAC		; QUOTATION MARK OR $00
	stx	ENDCHR


; ----------------------------------------------------------------------------
; BUILD A DESCRIPTOR FOR STRING STARTING AT Y,A
; AND TERMINATED BY $00, (CHARAC), OR (ENDCHR)
;
; RETURN WITH DESCRIPTOR IN A TEMPORARY
; AND ADDRESS OF DESCRIPTOR IN FAC+3,4
; ----------------------------------------------------------------------------
STRLT2:	sta	STRNG1		; SAVE ADDRESS OF STRING
	sty	STRNG1+1
	sta	FAC+1		; ...AGAIN
	sty	FAC+2
	ldy	#$FF
@1:	iny			; FIND END OF STRING
	lda	(STRNG1),y	; NEXT STRING CHAR
	beq	@3		; END OF STRING
	cmp	CHARAC		; ALTERNATE TERMINATOR # 1?
	beq	@2		; YES
	cmp	ENDCHR		; ALTERNATE TERMINATOR # 2?
	bne	@1		; NO, KEEP SCANNING
@2:	cmp	#'"'		; IS STRING ENDED WITH QUOTE MARK?
	beq	@4		; YES, C=1 TO INCLUDE " IN STRING
@3:	clc
@4:	sty	FAC		; SAVE LENGTH
	tya
	adc	STRNG1		; COMPUTE ADDRESS OF END OF STRING
	sta	STRNG2		;	(OF 00 BYTE, OR JUST AFTER ")
	ldx	STRNG1+1
	bcc	@5
	inx
@5:	stx	STRNG2+1
	lda	STRNG1+1	; WHERE DOES THE STRING START?
	beq	@6		; PAGE 0, MUST BE FROM STR$ FUNCTION
	cmp	#2		; PAGE 2?
	bne	PUTNEW		; NO, NOT PAGE 0 OR 2
@6:	tya			; LENGTH OF STRING
	jsr	STRINI		; MAKE SPACE FOR STRING
	ldx	STRNG1
	ldy	STRNG1+1
	jsr	MOVSTR		; MOVE IT IN


; ----------------------------------------------------------------------------
; STORE DESCRIPTOR IN TEMPORARY DESCRIPTOR STACK
;
; THE DESCRIPTOR IS NOW IN FAC, FAC+1, FAC+2
; PUT ADDRESS OF TEMP DESCRIPTOR IN FAC+3,4
; ----------------------------------------------------------------------------
PUTNEW:	ldx	TEMPPT		; POINTER TO NEXT TEMP STRING SLOT
	cpx	#TEMPST+9	; MAX OF 3 TEMP STRINGS
	bne	PUTEMP		; ROOM FOR ANOTHER ONE
	ldx	#ERR_FRMCPX	; TOO MANY, FORMULA TOO COMPLEX
JERR:	jmp	ERROR
; ----------------------------------------------------------------------------
PUTEMP:	lda	FAC		; COPY TEMP DESCRIPTOR INTO TEMP STACK
	sta	0,x
	lda	FAC+1
	sta	1,x
	lda	FAC+2
	sta	2,x
	ldy	#0
	stx	FAC+3		; ADDRESS OF TEMP DESCRIPTOR
	sty	FAC+4		; IN Y,X AND FAC+3,4
	dey			; Y=$FF
	sty	VALTYP		; FLAG (FAC ) AS STRING
	stx	LASTPT		; INDEX OF LAST POINTER
	inx			; UPDATE FOR NEXT TEMP ENTRY
	inx
	inx
	stx	TEMPPT
	rts


; ----------------------------------------------------------------------------
; MAKE SPACE FOR STRING AT BOTTOM OF STRING SPACE
; (A)=# BYTES SPACE TO MAKE
;
; RETURN WITH (A) SAME,
;	AND Y,X = ADDRESS OF SPACE ALLOCATED
; ----------------------------------------------------------------------------
GETSPA:	lsr	GARFLG		; CLEAR SIGNBIT OF FLAG
@1:	pha			; A HOLDS LENGTH
	eor	#$FF		; GET -LENGTH
	sec
	adc	FRETOP		; COMPUTE STARTING ADDRESS OF SPACE
	ldy	FRETOP+1	; FOR THE STRING
	bcs	@2
	dey
@2:	cpy	STREND+1	; SEE IF FITS IN REMAINING MEMORY
	bcc	@4		; NO, TRY GARBAGE
	bne	@3		; YES, IT FITS
	cmp	STREND		; HAVE TO CHECK LOWER BYTES
	bcc	@4		; NOT ENUF ROOM YET
@3:	sta	FRETOP		; THERE IS ROOM SO SAVE NEW FRETOP
	sty	FRETOP+1
	sta	FRESPC
	sty	FRESPC+1
	tax			; ADDR IN Y,X
	pla			; LENGTH IN A
	rts
@4:	ldx	#ERR_MEMFULL
	lda	GARFLG		; GARBAGE DONE YET?
	bmi	JERR		; YES, MEMORY IS REALLY FULL
	jsr	GARBAG		; NO, TRY COLLECTING NOW
	lda	#$80		; FLAG THAT COLLECTED GARBAGE ALREADY
	sta	GARFLG
	pla			; GET STRING LENGTH AGAIN
	bne	@1		; ...ALWAYS


; ----------------------------------------------------------------------------
; SHOVE ALL REFERENCED STRINGS AS HIGH AS POSSIBLE
; IN MEMORY (AGAINST HIMEM), FREEING UP SPACE
; BELOW STRING AREA DOWN TO STREND.
; ----------------------------------------------------------------------------
GARBAG:	ldx	MEMSIZ		; COLLECT FROM TOP DOWN
	lda	MEMSIZ+1
FINDHIGHESTSTRING:
	stx	FRETOP		; ONE PASS THROUGH ALL VARS
	sta	FRETOP+1	; FOR EACH ACTIVE STRING!
	ldy	#0
	sty	FNCNAM+1	; FLAG IN CASE NO STRINGS TO COLLECT
	lda	STREND
	ldx	STREND+1
	sta	LOWTR
	stx	LOWTR+1
; ----------------------------------------------------------------------------
; START BY COLLECTING TEMPORARIES
; ----------------------------------------------------------------------------
	lda	#<TEMPST
	ldx	#>TEMPST
	sta	INDEX
	stx	INDEX+1
@1:	cmp	TEMPPT		; FINISHED WITH TEMPS YET?
	beq	@2		; YES, NOW DO SIMPLE VARIABLES
	jsr	CHECK_VARIABLE	; DO A TEMP
	beq	@1		; ...ALWAYS
; ----------------------------------------------------------------------------
; NOW COLLECT SIMPLE VARIABLES
; ----------------------------------------------------------------------------
@2:	lda	#7		; LENGTH OF EACH VARIABLE IS 7 BYTES
	sta	DSCLEN
	lda	VARTAB		; START AT BEGINNING OF VARTAB
	ldx	VARTAB+1
	sta	INDEX
	stx	INDEX+1
@3:	cpx	ARYTAB+1	; FINISHED WITH SIMPLE VARIABLES?
	bne	@4		; NO
	cmp	ARYTAB		; MAYBE, CHECK LO-BYTE
	beq	@5		; YES, NOW DO ARRAYS
@4:	jsr	CHECK_SIMPLE_VARIABLE
	beq	@3		; ...ALWAYS
; ----------------------------------------------------------------------------
; NOW COLLECT ARRAY VARIABLES
; ----------------------------------------------------------------------------
@5:	sta	ARYPNT
	stx	ARYPNT+1
	lda	#3		; DESCRIPTORS IN ARRAYS ARE 3-BYTES EACH
	sta	DSCLEN
@6:	lda	ARYPNT		; COMPARE TO END OF ARRAYS
	ldx	ARYPNT+1
@7:	cpx	STREND+1	; FINISHED WITH ARRAYS YET?
	bne	@8		; NOT YET
	cmp	STREND		; MAYBE, CHECK LO-BYTE
	bne	@8		; NOT FINISHED YET
	jmp	MOVE_HIGHEST_STRING_TO_TOP	; FINISHED
@8:	sta	INDEX		; SET UP PNTR TO START OF ARRAY
	stx	INDEX+1
	ldy	#0		; POINT AT NAME OF ARRAY
	lda	(INDEX),y
	tax			; 1ST LETTER OF NAME IN X-REG
	iny
	lda	(INDEX),y
	php			; STATUS FROM SECOND LETTER OF NAME
	iny
	lda	(INDEX),y	; OFFSET TO NEXT ARRAY
	adc	ARYPNT		; (CARRY ALWAYS CLEAR)
	sta	ARYPNT		; CALCULATE START OF NEXT ARRAY
	iny
	lda	(INDEX),y	; HI-BYTE OF OFFSET
	adc	ARYPNT+1
	sta	ARYPNT+1
	plp			; GET STATUS FROM 2ND CHAR OF NAME
	bpl	@6		; NOT A STRING ARRAY
	txa			; SET STATUS WITH 1ST CHAR OF NAME
	bmi	@6		; NOT A STRING ARRAY
	iny
	lda	(INDEX),y	; # OF DIMENSIONS FOR THIS ARRAY
	ldy	#0
	asl	a		; PREAMBLE SIZE = 2*#DIMS + 5
	adc	#5
	adc	INDEX		; MAKE INDEX POINT AT FIRST ELEMENT
	sta	INDEX		; 	IN THE ARRAY
	bcc	@9
	inc	INDEX+1
@9:	ldx	INDEX+1		; STEP THRU EACH STRING IN THIS ARRAY
@10:	cpx	ARYPNT+1	; ARRAY DONE?
	bne	@11		; NO, PROCESS NEXT ELEMENT
	cmp	ARYPNT		; MAYBE, CHECK LO-BYTE
	beq	@7		; YES, MOVE TO NEXT ARRAY
@11:	jsr	CHECK_VARIABLE	; PROCESS THE ARRAY
	beq	@10		; ...ALWAYS


; ----------------------------------------------------------------------------
; PROCESS A SIMPLE VARIABLE
; ----------------------------------------------------------------------------
CHECK_SIMPLE_VARIABLE:
	lda	(INDEX),y	; LOOK AT 1ST CHAR OF NAME
	bmi	CHECK_BUMP	; NOT A STRING VARIABLE
	iny
	lda	(INDEX),y	; LOOK AT 2ND CHAR OF NAME
	bpl	CHECK_BUMP	; NOT A STRING VARIABLE
	iny


; ----------------------------------------------------------------------------
; IF STRING IS NOT EMPTY, CHECK IF IT IS HIGHEST
; ----------------------------------------------------------------------------
CHECK_VARIABLE:
	lda	(INDEX),y	; GET LENGTH OF STRING
	beq	CHECK_BUMP	; IGNORE STRING IF LENGTH IS ZERO
	iny
	lda	(INDEX),y	; GET ADDRESS OF STRING
	tax
	iny
	lda	(INDEX),y
	cmp	FRETOP+1	; CHECK IF ALREADY COLLECTED
	bcc	@1		; NO, BELOW FRETOP
	bne	CHECK_BUMP	; YES, ABOVE FRETOP
	cpx	FRETOP		; MAYBE, CHECK LO-BYTE
	bcs	CHECK_BUMP	; YES, ABOVE FRETOP
@1:	cmp	LOWTR+1		; ABOVE HIGHEST STRING FOUND?
	bcc	CHECK_BUMP	; NO, IGNORE FOR NOW
	bne	@2		; YES, THIS IS THE NEW HIGHEST
	cpx	LOWTR		; MAYBE, TRY LO-BYTE
	bcc	CHECK_BUMP	; NO, IGNORE FOR NOW
@2:	stx	LOWTR		; MAKE THIS THE HIGHEST STRING
	sta	LOWTR+1
	lda	INDEX		; SAVE ADDRESS OF DESCRIPTOR TOO
	ldx	INDEX+1
	sta	FNCNAM
	stx	FNCNAM+1
	lda	DSCLEN
	sta	LENGTH


; ----------------------------------------------------------------------------
; ADD (DSCLEN) TO PNTR IN INDEX
; RETURN WITH Y=0, PNTR ALSO IN X,A
; ----------------------------------------------------------------------------
CHECK_BUMP:
	lda	DSCLEN		; BUMP TO NEXT VARIABLE
	clc
	adc	INDEX
	sta	INDEX
	bcc	CHECK_EXIT
	inc	INDEX+1
; ----------------------------------------------------------------------------
CHECK_EXIT:
	ldx	INDEX+1
	ldy	#0
	rts


; ----------------------------------------------------------------------------
; FOUND HIGHEST NON-EMPTY STRING, SO MOVE IT
; TO TOP AND GO BACK FOR ANOTHER
; ----------------------------------------------------------------------------
MOVE_HIGHEST_STRING_TO_TOP:
	ldx	FNCNAM+1	; ANY STRING FOUND?
	beq	CHECK_EXIT	; NO, RETURN
	lda	LENGTH		; GET LENGTH OF VARIABLE ELEMENT
	and	#4		; WAS 7 OR 3, MAKE 4 OR 0
	lsr	a		; 2 0R 0; IN SIMPLE VARIABLES,
	tay			; NAME PRECEDES DESCRIPTOR
	sta	LENGTH		; 2 OR 0
	lda	(FNCNAM),y	; GET LENGTH FROM DESCRIPTOR
	adc	LOWTR		; CARRY ALREADY CLEARED BY LSR
	sta	HIGHTR		; STRING IS BTWN (LOWTR) AND (HIGHTR)
	lda	LOWTR+1
	adc	#0
	sta	HIGHTR+1
	lda	FRETOP		; HIGH END DESTINATION
	ldx	FRETOP+1
	sta	HIGHDS
	stx	HIGHDS+1
	jsr	BLTU2		; MOVE STRING UP
	ldy	LENGTH		; FIX ITS DESCRIPTOR
	iny			; POINT AT ADDRESS IN DESCRIPTOR
	lda	HIGHDS		; STORE NEW ADDRESS
	sta	(FNCNAM),y
	tax
	inc	HIGHDS+1	; CORRECT BLTU'S OVERSHOOT
	lda	HIGHDS+1
	iny
	sta	(FNCNAM),y
	jmp	FINDHIGHESTSTRING


; ----------------------------------------------------------------------------
; CONCATENATE TWO STRINGS
; ----------------------------------------------------------------------------
CAT:	lda	FAC+4		; SAVE ADDRESS OF FIRST DESCRIPTOR
	pha
	lda	FAC+3
	pha
	jsr	FRM_ELEMENT	; GET SECOND STRING ELEMENT
	jsr	CHKSTR		; MUST BE A STRING
	pla			; RECOVER ADDRES OF 1ST DESCRIPTOR
	sta	STRNG1
	pla
	sta	STRNG1+1
	ldy	#0
	lda	(STRNG1),y	; ADD LENGTHS, GET CONCATENATED SIZE
	clc
	adc	(FAC+3),y
	bcc	@1		; OK IF < $100
	ldx	#ERR_STRLONG
	jmp	ERROR
@1:	jsr	STRINI		; GET SPACE FOR CONCATENATED STRINGS
	jsr	MOVINS		; MOVE 1ST STRING
	lda	DSCPTR
	ldy	DSCPTR+1
	jsr	FRETMP
	jsr	MOVSTR1		; MOVE 2ND STRING
	lda	STRNG1
	ldy	STRNG1+1
	jsr	FRETMP
	jsr	PUTNEW		; SET UP DESCRIPTOR
	jmp	FRMEVL2		; FINISH EXPRESSION


; ----------------------------------------------------------------------------
; GET STRING DESCRIPTOR POINTED AT BY (STRNG1)
; AND MOVE DESCRIBED STRING TO (FRESPC)
; ----------------------------------------------------------------------------
MOVINS:	ldy	#0
	lda	(STRNG1),y
	pha			; LENGTH
	iny
	lda	(STRNG1),y
	tax			; PUT STRING POINTER IN X,Y
	iny
	lda	(STRNG1),y
	tay
	pla			; RETRIEVE LENGTH


; ----------------------------------------------------------------------------
; MOVE STRING AT (Y,X) WITH LENGTH (A)
; TO DESTINATION WHOSE ADDRESS IS IN FRESPC,FRESPC+1
; ----------------------------------------------------------------------------
MOVSTR:	stx	INDEX		; PUT POINTER IN INDEX
	sty	INDEX+1
MOVSTR1:
	tay			; LENGTH TO Y-REG
	beq	@2		; IF LENGTH IS ZERO, FINISHED
	pha			; SAVE LENGTH ON STACK
@1:	dey			; MOVE BYTES FROM (INDEX) TO (FRESPC)
	lda	(INDEX),y
	sta	(FRESPC),y
	tya			; TEST IF ANY LEFT TO MOVE
	bne	@1		; YES, KEEP MOVING
	pla			; NO, FINISHED.  GET LENGTH
@2:	clc			; AND ADD TO FRESPC, SO
	adc	FRESPC		; FRESPC POINTS TO NEXT HIGHER
	sta	FRESPC		; BYTE.  (USED BY CONCATENATION)
	bcc	@4
	inc	FRESPC+1
@4:	rts


; ----------------------------------------------------------------------------
; IF (FAC) IS A TEMPORARY STRING, RELEASE DESCRIPTOR
; ----------------------------------------------------------------------------
FRESTR:	jsr	CHKSTR		; LAST RESULT A STRING?


; ----------------------------------------------------------------------------
; IF STRING DESCRIPTOR POINTED TO BY FAC+3,4 IS
; A TEMPORARY STRING, RELEASE IT.
; ----------------------------------------------------------------------------
FREFAC:	lda	FAC+3		; GET DESCRIPTOR POINTER
	ldy	FAC+4


; ----------------------------------------------------------------------------
; IF STRING DESCRIPTOR WHOSE ADDRESS IS IN Y,A IS
; A TEMPORARY STRING, RELEASE IT.
; ----------------------------------------------------------------------------
FRETMP:	sta	INDEX		; SAVE THE ADDRESS OF THE DESCRIPTOR
	sty	INDEX+1
	jsr	FRETMS		; FREE DESCRIPTOR IF IT IS TEMPORARY
	php			; REMEMBER IF TEMP
	ldy	#0		; POINT AT LENGTH OF STRING
	lda	(INDEX),y
	pha			; SAVE LENGTH ON STACK
	iny
	lda	(INDEX),y
	tax			; GET ADDRESS OF STRING IN Y,X
	iny
	lda	(INDEX),y
	tay
	pla			; LENGTH IN A
	plp			; RETRIEVE STATUS, Z=1 IF TEMP
	bne	@2		; NOT A TEMPORARY STRING
	cpy	FRETOP+1	; IS IT THE LOWEST STRING?
	bne	@2		; NO
	cpx	FRETOP
	bne	@2		; NO
	pha			; YES, PUSH LENGTH AGAIN
	clc			; RECOVER THE SPACE USED BY
	adc	FRETOP		; THE STRING
	sta	FRETOP
	bcc	@1
	inc	FRETOP+1
@1:	pla			; RETRIEVE LENGTH AGAIN
@2:	stx	INDEX		; ADDRESS OF STRING IN Y,X
	sty	INDEX+1		; LENGTH OF STRING IN A-REG
	rts


; ----------------------------------------------------------------------------
; RELEASE TEMPORARY DESCRIPTOR IF Y,A = LASTPT
; ----------------------------------------------------------------------------
FRETMS:	cpy	LASTPT+1	; COMPARE Y,A TO LATEST TEMP
	bne	@1		; NOT SAME ONE, CANNOT RELEASE
	cmp	LASTPT
	bne	@1		; NOT SAME ONE, CANNOT RELEASE
	sta	TEMPPT		; UPDATE TEMPT FOR NEXT TEMP
	sbc	#3		; BACK OFF LASTPT
	sta	LASTPT
	ldy	#0		; NOW Y,A POINTS TO TOP TEMP
@1:	rts			; Z=0 IF NOT TEMP, Z=1 IF TEMP


; ----------------------------------------------------------------------------
; "CHR$" FUNCTION
; ----------------------------------------------------------------------------
CHRSTR:	jsr	CONINT		; CONVERT ARGUMENT TO BYTE IN X
	txa
	pha			; SAVE IT
	lda	#1		; GET SPACE FOR STRING OF LENGTH 1
	jsr	STRSPA
	pla			; RECALL THE CHARACTER
	ldy	#0		; PUT IN STRING
	sta	(FAC+1),y
	pla			; POP RETURN ADDRESS
	pla
	jmp	PUTNEW		; MAKE IT A TEMPORARY STRING


; ----------------------------------------------------------------------------
; "LEFT$" FUNCTION
; ----------------------------------------------------------------------------
LEFTSTR:
	jsr	SUBSTRING_SETUP
	cmp	(DSCPTR),y	; COMPARE 1ST PARAMETER TO LENGTH
	tya			; Y=A=0
SUBSTRING1:
	bcc	@1		; 1ST PARAMETER SMALLER, USE IT
	lda	(DSCPTR),y	; 1ST IS LONGER, USE STRING LENGTH
	tax			; IN X-REG
	tya			; Y=A=0 AGAIN
@1:	pha			; PUSH LEFT END OF SUBSTRING
SUBSTRING2:
	txa
SUBSTRING3:
	pha			; PUSH LENGTH OF SUBSTRING
	jsr	STRSPA		; MAKE ROOM FOR STRING OF (A) BYTES
	lda	DSCPTR		; RELEASE PARAMETER STRING IF TEMP
	ldy	DSCPTR+1
	jsr	FRETMP
	pla			; GET LENGTH OF SUBSTRING
	tay			; IN Y-REG
	pla			; GET LEFT END OF SUBSTRING
	clc			; ADD TO POINTER TO STRING
	adc	INDEX
	sta	INDEX
	bcc	@1
	inc	INDEX+1
@1:	tya			; LENGTH
	jsr	MOVSTR1		; COPY STRING INTO SPACE
	jmp	PUTNEW		; ADD TO TEMPS


; ----------------------------------------------------------------------------
; "RIGHT$" FUNCTION
; ----------------------------------------------------------------------------
RIGHTSTR:
	jsr	SUBSTRING_SETUP
	clc			; COMPUTE LENGTH-WIDTH OF SUBSTRING
	sbc	(DSCPTR),y	; TO GET STARTING POINT IN STRING
	eor	#$FF
	jmp	SUBSTRING1	; JOIN LEFT$


; ----------------------------------------------------------------------------
; "MID$" FUNCTION
; ----------------------------------------------------------------------------
MIDSTR:	lda	#$FF		; FLAG WHETHER 2ND PARAMETER
	sta	FAC+4
	jsr	CHRGOT		; SEE IF ")" YET
	cmp	#')'
	beq	@1		; YES, NO 2ND PARAMETER
	jsr	CHKCOM		; NO, MUST HAVE COMMA
	jsr	GETBYT		; GET 2ND PARAM IN X-REG
@1:	jsr	SUBSTRING_SETUP
	dex			; 1ST PARAMETER - 1
	txa
	pha
	clc
	ldx	#0
	sbc	(DSCPTR),y
	bcs	SUBSTRING2
	eor	#$FF
	cmp	FAC+4		; USE SMALLER OF TWO
	bcc	SUBSTRING3
	lda	FAC+4
	bcs	SUBSTRING3	; ...ALWAYS


; ----------------------------------------------------------------------------
; COMMON SETUP ROUTINE FOR LEFT$, RIGHT$, MID$:
; REQUIRE ")"; POP RETURN ADRS, GET DESCRIPTOR
; ADDRESS, GET 1ST PARAMETER OF COMMAND
; ----------------------------------------------------------------------------
SUBSTRING_SETUP:
	jsr	CHKCLS		; REQUIRE ")"
	pla			; SAVE RETURN ADDRESS
	tay			; IN Y-REG AND LENGTH
	pla
	sta	LENGTH
	pla			; POP PREVIOUS RETURN ADDRESS
	pla			; (FROM GOROUT).
	pla			; RETRIEVE 1ST PARAMETER
	tax
	pla			; GET ADDRESS OF STRING DESCRIPTOR
	sta	DSCPTR
	pla
	sta	DSCPTR+1
	lda	LENGTH		; RESTORE RETURN ADDRESS
	pha
	tya
	pha
	ldy	#0
	txa			; GET 1ST PARAMETER IN A-REG
	beq	GOIQ		; ERROR IF 0
	rts


; ----------------------------------------------------------------------------
; "LEN" FUNCTION
; ----------------------------------------------------------------------------
LEN:	jsr	GETSTR		; GET LENTGH IN Y-REG, MAKE FAC NUMERIC
	jmp	SNGFLT		; FLOAT Y-REG INTO FAC


; ----------------------------------------------------------------------------
; IF LAST RESULT IS A TEMPORARY STRING, FREE IT
; MAKE VALTYP NUMERIC, RETURN LENGTH IN Y-REG
; ----------------------------------------------------------------------------
GETSTR:	jsr	FRESTR		; IF LAST RESULT IS A STRING, FREE IT
	ldx	#0		; MAKE VALTYP NUMERIC
	stx	VALTYP
	tay			; LENGTH OF STRING TO Y-REG
	rts


; ----------------------------------------------------------------------------
; "ASC" FUNCTION
; ----------------------------------------------------------------------------
ASC:	jsr	GETSTR		; GET STRING, GET LENGTH IN Y-REG
	beq	GOIQ		; ERROR IF LENGTH 0
	ldy	#0
	lda	(INDEX),y	; GET 1ST CHAR OF STRING
	tay
	jmp	SNGFLT		; FLOAT Y-REG INTO FAC
; ----------------------------------------------------------------------------
GOIQ:	jmp	IQERR		; ILLEGAL QUANTITY ERROR


; ----------------------------------------------------------------------------
; SCAN TO NEXT CHARACTER AND CONVERT EXPRESSION
; TO SINGLE BYTE IN X-REG
; ----------------------------------------------------------------------------
GTBYTC:	jsr	CHRGET


; ----------------------------------------------------------------------------
; EVALUATE EXPRESSION AT TXTPTR, AND
; CONVERT IT TO SINGLE BYTE IN X-REG
; ----------------------------------------------------------------------------
GETBYT:	jsr	FRMNUM


; ----------------------------------------------------------------------------
; CONVERT (FAC) TO SINGLE BYTE INTEGER IN X-REG
; ----------------------------------------------------------------------------
CONINT:	jsr	MKINT		; CONVERT IF IN RANGE -32767 TO +32767
	ldx	FAC+3		; HI-BYTE MUST BE ZERO
	bne	GOIQ		; VALUE > 255, ERROR
	ldx	FAC+4		; VALUE IN X-REG
	jmp	CHRGOT		; GET NEXT CHAR IN A-REG


; ----------------------------------------------------------------------------
; "VAL" FUNCTION
; ----------------------------------------------------------------------------
VAL:	jsr	GETSTR		; GET POINTER TO STRING IN INDEX
	bne	@1		; LENGTH NON-ZERO
	jmp	ZERO_FAC	; RETURN 0 IF LENGTH=0
@1:	ldx	TXTPTR		; SAVE CURRENT TXTPTR
	ldy	TXTPTR+1
	stx	STRNG2
	sty	STRNG2+1
	ldx	INDEX
	stx	TXTPTR		; POINT TXTPTR TO START OF STRING
	clc
	adc	INDEX		; ADD LENGTH
	sta	DEST		; POINT DEST TO END OF STRING + 1
	ldx	INDEX+1
	stx	TXTPTR+1
	bcc	@2
	inx
@2:	stx	DEST+1
	ldy	#0		; SAVE BYTE THAT FOLLOWS STRING
	lda	(DEST),y	; ON STACK
	pha
	lda	#0		; AND STORE $00 IN ITS PLACE
	sta	(DEST),y
	jsr	CHRGOT		; PRIME THE PUMP
	jsr	FIN		; EVALUATE STRING
	pla			; GET BYTE THAT SHOULD FOLLOW STRING
	ldy	#0		; AND PUT IT BACK
	sta	(DEST),y


; ----------------------------------------------------------------------------
; COPY STRNG2 INTO TXTPTR
; ----------------------------------------------------------------------------
POINT:	ldx	STRNG2
	ldy	STRNG2+1
	stx	TXTPTR
	sty	TXTPTR+1
	rts


; ----------------------------------------------------------------------------
; EVALUATE "EXP1,EXP2"
;
; CONVERT EXP1 TO 16-BIT NUMBER IN LINNUM
; CONVERT EXP2 TO 8-BIT NUMBER IN X-REG
; ----------------------------------------------------------------------------
GTNUM:	jsr	FRMNUM
	jsr	GETADR


; ----------------------------------------------------------------------------
; EVALUATE ",EXPRESSION"
; CONVERT EXPRESSION TO SINGLE BYTE IN X-REG
; ----------------------------------------------------------------------------
COMBYTE:
	jsr	CHKCOM		; MUST HAVE COMMA FIRST
	jmp	GETBYT		; CONVERT EXPRESSION TO BYTE IN X-REG


; ----------------------------------------------------------------------------
; CONVERT (FAC) TO A 16-BIT VALUE IN LINNUM
; ----------------------------------------------------------------------------
GETADR:	lda	FAC		; FAC < 2^16?
	cmp	#$91
	bcs	GOIQ		; NO, ILLEGAL QUANTITY
	jsr	QINT		; CONVERT TO INTEGER
	lda	FAC+3		; COPY IT INTO LINNUM
	ldy	FAC+4
	sty	LINNUM		; TO LINNUM
	sta	LINNUM+1
	rts


; ----------------------------------------------------------------------------
; "PEEK" FUNCTION
; ----------------------------------------------------------------------------
PEEK:	lda	LINNUM		; SAVE (LINNUM) ON STACK DURING PEEK
	pha
	lda	LINNUM+1
	pha
	jsr	GETADR		; GET ADDRESS PEEKING AT
	ldy	#0
	lda	(LINNUM),y	; TAKE A QUICK LOOK
	tay			; VALUE IN Y-REG
	pla			; RESTORE LINNUM FROM STACK
	sta	LINNUM+1
	pla
	sta	LINNUM
	jmp	SNGFLT		; FLOAT Y-REG INTO FAC


; ----------------------------------------------------------------------------
; "POKE" STATEMENT
; ----------------------------------------------------------------------------
POKE:	jsr	GTNUM		; GET THE ADDRESS AND VALUE
	txa			; VALUE IN A,
	ldy	#0
	sta	(LINNUM),y	; STORE IT AWAY,
RTS10:	rts			; AND THAT'S ALL FOR TODAY


; ----------------------------------------------------------------------------
; ADD 0.5 TO FAC
; ----------------------------------------------------------------------------
FADDH:	lda	#<CON_HALF	; FAC+1/2 -> FAC
	ldy	#>CON_HALF
	jmp	FADD


; ----------------------------------------------------------------------------
; FAC = (Y,A) - FAC
; ----------------------------------------------------------------------------
FSUB:	jsr	LOAD_ARG_FROM_YA


; ----------------------------------------------------------------------------
; FAC = ARG - FAC
; ----------------------------------------------------------------------------
FSUBT:	lda	FACSIGN		; COMPLEMENT FAC AND ADD
	eor	#$FF
	sta	FACSIGN
	eor	ARGSIGN		; FIX SGNCPR TOO
	sta	SGNCPR
	lda	FAC		; MAKE STATUS SHOW FAC EXPONENT
	jmp	FADDT		; JOIN FADD


; ----------------------------------------------------------------------------
; SHIFT SMALLER ARGUMENT MORE THAN 7 BITS
; ----------------------------------------------------------------------------
FADD1:	jsr	SHIFT_RIGHT	; ALIGN RADIX BY SHIFTING
	bcc	FADD3		; ...ALWAYS


; ----------------------------------------------------------------------------
; FAC = (Y,A) + FAC
; ----------------------------------------------------------------------------
FADD:	jsr	LOAD_ARG_FROM_YA


; ----------------------------------------------------------------------------
; FAC = ARG + FAC
; ----------------------------------------------------------------------------
FADDT:	bne	@1		; FAC IS NON-ZERO
	jmp	COPY_ARG_TO_FAC	; FAC = 0 + ARG
@1:	ldx	FACEXTENSION
	stx	ARGEXTENSION
	ldx	#ARG		; SET UP TO SHIFT ARG
	lda	ARG		; EXPONENT

; ----------------------------------------------------------------------------
FADD2:	tay
	beq	RTS10		; IF ARG=0, WE ARE FINISHED
	sec
	sbc	FAC		; GET DIFFNCE OF EXP
	beq	FADD3		; GO ADD IF SAME EXP
	bcc	@1		; ARG HAS SMALLER EXPONENT
	sty	FAC		; EXP HAS SMALLER EXPONENT
	ldy	ARGSIGN
	sty	FACSIGN
	eor	#$FF		; COMPLEMENT SHIFT COUNT
	adc	#0		; CARRY WAS SET
	ldy	#0
	sty	ARGEXTENSION
	ldx	#FAC		; SET UP TO SHIFT FAC
	bne	@2		; ...ALWAYS
@1:	ldy	#0
	sty	FACEXTENSION
@2:	cmp	#$F9		; SHIFT MORE THAN 7 BITS?
	bmi	FADD1		; YES
	tay			; INDEX TO # OF SHIFTS
	lda	FACEXTENSION
	lsr	1,x		; START SHIFTING...
	jsr	SHIFT_RIGHT4	; ...COMPLETE SHIFTING
FADD3:  bit	SGNCPR		; DO FAC AND ARG HAVE SAME SIGNS?
	bpl	FADD4		; YES, ADD THE MANTISSAS
	ldy	#FAC		; NO, SUBTRACT SMALLER FROM LARGER
	cpx	#ARG		; WHICH WAS ADJUSTED?
	beq	@1		; IF ARG, DO FAC-ARG
	ldy	#ARG		; IF FAC, DO ARG-FAC
@1:	sec			; SUBTRACT SMALLER FROM LARGER (WE HOPE)
	eor	#$FF		; (IF EXPONENTS WERE EQUAL, WE MIGHT BE
	adc	ARGEXTENSION	; SUBTRACTING LARGER FROM SMALLER)
	sta	FACEXTENSION
	lda	4,y
	sbc	4,x
	sta	FAC+4
	lda	3,y
	sbc	3,x
	sta	FAC+3
	lda	2,y
	sbc	2,x
	sta	FAC+2
	lda	1,y
	sbc	1,x
	sta	FAC+1


; ----------------------------------------------------------------------------
; NORMALIZE VALUE IN FAC
; ----------------------------------------------------------------------------
NORMALIZE_FAC1:
	bcs	NORMALIZE_FAC2
	jsr	COMPLEMENT_FAC
; ----------------------------------------------------------------------------
NORMALIZE_FAC2:
	ldy	#0		; SHIFT UP SIGNIF DIGIT
	tya			; START A=0, COUNT SHIFTS IN A-REG
	clc
@1:	ldx	FAC+1		; LOOK AT MOST SIGNIFICANT BYTE
	bne	NORMALIZE_FAC4	; SOME 1-BITS HERE
	ldx	FAC+2		; HI-BYTE OF MANTISSA STILL ZERO,
	stx	FAC+1		; SO DO A FAST 8-BIT SHUFFLE
	ldx	FAC+3
	stx	FAC+2
	ldx	FAC+4
	stx	FAC+3
	ldx	FACEXTENSION
	stx	FAC+4
	sty	FACEXTENSION	; ZERO EXTENSION BYTE
	adc	#8		; BUMP SHIFT COUNT
	cmp	#32		; DONE 4 TIMES YET?
	bne	@1		; NO, STILL MIGHT BE SOME 1'S (YES, VALUE OF FAC IS ZERO)


; ----------------------------------------------------------------------------
; SET FAC = 0
; (ONLY NECESSARY TO ZERO EXPONENT AND SIGN CELLS)
; ----------------------------------------------------------------------------
ZERO_FAC:
	lda	#0
; ----------------------------------------------------------------------------
STA_IN_FAC_SIGN_AND_EXP:
	sta	FAC
; ----------------------------------------------------------------------------
STA_IN_FAC_SIGN:
	sta	FACSIGN
	rts


; ----------------------------------------------------------------------------
; ADD MANTISSAS OF FAC AND ARG INTO FAC
; ----------------------------------------------------------------------------
FADD4:	adc	ARGEXTENSION
	sta	FACEXTENSION
	lda	FAC+4
	adc	ARG+4
	sta	FAC+4
	lda	FAC+3
	adc	ARG+3
	sta	FAC+3
	lda	FAC+2
	adc	ARG+2
	sta	FAC+2
	lda	FAC+1
	adc	ARG+1
	sta	FAC+1
	jmp	NORMALIZE_FAC5


; ----------------------------------------------------------------------------
; FINISH NORMALIZING FAC
; ----------------------------------------------------------------------------
NORMALIZE_FAC3:
	adc	#1		; COUNT BITS SHIFTED
	asl	FACEXTENSION
	rol	FAC+4
	rol	FAC+3
	rol	FAC+2
	rol	FAC+1
; ----------------------------------------------------------------------------
NORMALIZE_FAC4:
	bpl	NORMALIZE_FAC3	; UNTIL TOP BIT = 1
	sec
	sbc	FAC		; ADJUST EXPONENT BY BITS SHIFTED
	bcs	ZERO_FAC	; UNDERFLOW, RETURN ZERO
	eor	#$FF
	adc	#1		; 2'S COMPLEMENT
	sta	FAC		; CARRY=0 NOW
; ----------------------------------------------------------------------------
NORMALIZE_FAC5:
	bcc	RTS11		; UNLESS MANTISSA CARRIED
; ----------------------------------------------------------------------------
NORMALIZE_FAC6:
	inc	FAC		; MANTISSA CARRIED, SO SHIFT RIGHT
	beq	OVERFLOW	; OVERFLOW IF EXPONENT TOO BIG
	ror	FAC+1
	ror	FAC+2
	ror	FAC+3
	ror	FAC+4
	ror	FACEXTENSION
RTS11:	rts


; ----------------------------------------------------------------------------
; 2'S COMPLEMENT OF FAC
; ----------------------------------------------------------------------------
COMPLEMENT_FAC:
	lda	FACSIGN
	eor	#$FF
	sta	FACSIGN


; ----------------------------------------------------------------------------
; 2'S COMPLEMENT OF FAC MANTISSA ONLY
; ----------------------------------------------------------------------------
COMPLEMENT_FAC_MANTISSA:
	lda	FAC+1
	eor	#$FF
	sta	FAC+1
	lda	FAC+2
	eor	#$FF
	sta	FAC+2
	lda	FAC+3
	eor	#$FF
	sta	FAC+3
	lda	FAC+4
	eor	#$FF
	sta	FAC+4
	lda	FACEXTENSION
	eor	#$FF
	sta	FACEXTENSION
	inc	FACEXTENSION	; START INCREMENTING MANTISSA
	bne	RTS12


; ----------------------------------------------------------------------------
; INCREMENT FAC MANTISSA
; ----------------------------------------------------------------------------
INCREMENT_FAC_MANTISSA:
	inc	FAC+4		; ADD CARRY FROM EXTRA
	bne	RTS12
	inc	FAC+3
	bne	RTS12
	inc	FAC+2
	bne	RTS12
	inc	FAC+1
RTS12:	rts
; ----------------------------------------------------------------------------
OVERFLOW:
	ldx	#ERR_OVERFLOW
	jmp	ERROR


; ----------------------------------------------------------------------------
; SHIFT 1,X THRU 5,X RIGHT
; (A) = NEGATIVE OF SHIFT COUNT
; (X) = POINTER TO BYTES TO BE SHIFTED
;
; RETURN WITH (Y)=0, CARRY=0, EXTENSION BITS IN A-REG
; ----------------------------------------------------------------------------
SHIFT_RIGHT1:
	ldx	#RESULT-1	; SHIFT RESULT RIGHT
SHIFT_RIGHT2:
	ldy	4,x		; SHIFT 8 BITS RIGHT
	sty	FACEXTENSION
	ldy	3,x
	sty	4,x
	ldy	2,x
	sty	3,x
	ldy	1,x
	sty	2,x
	ldy	SHIFTSIGNEXT	; $00 IF +, $FF IF -
	sty	1,x


; ----------------------------------------------------------------------------
; MAIN ENTRY TO RIGHT SHIFT SUBROUTINE
; ----------------------------------------------------------------------------
SHIFT_RIGHT:
	adc	#8
	bmi	SHIFT_RIGHT2	; STILL MORE THAN 8 BITS TO GO
	beq	SHIFT_RIGHT2	; EXACTLY 8 MORE BITS TO GO
	sbc	#8		; UNDO ADC ABOVE
	tay			; REMAINING SHIFT COUNT
	lda	FACEXTENSION
	bcs	SHIFT_RIGHT5	; FINISHED SHIFTING
SHIFT_RIGHT3:
	asl	1,x		; SIGN -> CARRY (SIGN EXTENSION)
	bcc	@1		; SIGN +
	inc	1,x		; PUT SIGN IN LSB
@1:	ror	1,x		; RESTORE VALUE, SIGN STILL IN CARRY
	ror	1,x		; START RIGHT SHIFT, INSERTING SIGN


; ----------------------------------------------------------------------------
; ENTER HERE FOR SHORT SHIFTS WITH NO SIGN EXTENSION
; ----------------------------------------------------------------------------
SHIFT_RIGHT4:
	ror	2,x
	ror	3,x
	ror	4,x
	ror	a		; EXTENSION
	iny			; COUNT THE SHIFT
	bne	SHIFT_RIGHT3
SHIFT_RIGHT5:
	clc			; RETURN WITH CARRY CLEAR
	rts


; ----------------------------------------------------------------------------
CON_ONE:
	.byte	$81,$00,$00,$00,$00
; ----------------------------------------------------------------------------
POLY_LOG:
	.byte	$03			; # OF COEFFICIENTS - 1
	.byte	$7F,$5E,$56,$CB,$79	; * X^7 +
	.byte	$80,$13,$9B,$0B,$64	; * X^5 +
	.byte	$80,$76,$38,$93,$16	; * X^3 +
	.byte	$82,$38,$AA,$3B,$20	; * X
; ----------------------------------------------------------------------------
CON_SQR_HALF:
	.byte	$80,$35,$04,$F3,$34
CON_SQR_TWO:
	.byte	$81,$35,$04,$F3,$34
CON_NEG_HALF:
	.byte	$80,$80,$00,$00,$00
CON_LOG_TWO:
	.byte	$80,$31,$72,$17,$F8


; ----------------------------------------------------------------------------
; "LOG" FUNCTION
; ----------------------------------------------------------------------------
LOG:    jsr     SIGN		; GET -1,0,+1 IN A-REG FOR FAC
        beq     GIQ		; LOG (0) IS ILLEGAL
        bpl     LOG2		; >0 IS OK
GIQ:    jmp     IQERR		; <= 0 IS NO GOOD
LOG2:   lda     FAC		; FIRST GET LOG BASE 2
        sbc     #$7F		; SAVE UNBIASED EXPONENT
        pha
        lda     #$80		; NORMALIZE BETWEEN .5 AND 1
        sta     FAC
        lda     #<CON_SQR_HALF
        ldy     #>CON_SQR_HALF
        jsr     FADD		; COMPUTE VIA SERIES OF ODD
        lda     #<CON_SQR_TWO	; POWERS OF
        ldy     #>CON_SQR_TWO	; (SQR(2)X-1)/(SQR(2)X+1)
        jsr     FDIV
        lda     #<CON_ONE
        ldy     #>CON_ONE
        jsr     FSUB
        lda     #<POLY_LOG
        ldy     #>POLY_LOG
        jsr     POLYNOMIAL_ODD
        lda     #<CON_NEG_HALF
        ldy     #>CON_NEG_HALF
        jsr     FADD
        pla
        jsr     ADDACC		; ADD ORIGINAL EXPONENT
        lda     #<CON_LOG_TWO	; MULTIPLY BY LOG(2) TO FORM
        ldy     #>CON_LOG_TWO	; NATURAL LOG OF X


; ----------------------------------------------------------------------------
; FAC = (Y,A) * FAC
; ----------------------------------------------------------------------------
FMULT:	jsr	LOAD_ARG_FROM_YA


; ----------------------------------------------------------------------------
; FAC = ARG * FAC
; ----------------------------------------------------------------------------
FMULTT:	bne	@1		; FAC .NE. ZERO
	rts			; FAC = 0 * ARG = 0
@1:	jsr	ADD_EXPONENTS
	lda	#0
	sta	RESULT		; INIT PRODUCT = 0
	sta	RESULT+1
	sta	RESULT+2
	sta	RESULT+3
	lda	FACEXTENSION
	jsr	MULTIPLY1
	lda	FAC+4
	jsr	MULTIPLY1
	lda	FAC+3
	jsr	MULTIPLY1
	lda	FAC+2
	jsr	MULTIPLY1
	lda	FAC+1
	jsr	MULTIPLY2
	jmp	COPY_RESULT_INTO_FAC


; ----------------------------------------------------------------------------
; MULTIPLY ARG BY (A) INTO RESULT
; ----------------------------------------------------------------------------
MULTIPLY1:
	bne	MULTIPLY2	; THIS BYTE NON-ZERO
	jmp	SHIFT_RIGHT1	; (A)=0, JUST SHIFT ARG RIGHT 8
; ----------------------------------------------------------------------------
MULTIPLY2:
	lsr 	a		; SHIFT BIT INTO CARRY
	ora	#$80		; SUPPLY SENTINEL BIT
@1:	tay			; REMAINING MULTIPLIER TO Y
	bcc	@2		; THIS MULTIPLIER BIT = 0
	clc			; = 1, SO ADD ARG TO RESULT
	lda	RESULT+3
	adc	ARG+4
	sta	RESULT+3
	lda	RESULT+2
	adc	ARG+3
	sta	RESULT+2
	lda	RESULT+1
	adc	ARG+2
	sta	RESULT+1
	lda	RESULT
	adc	ARG+1
	sta	RESULT
@2:	ror	RESULT		; SHIFT RESULT RIGHT 1
	ror	RESULT+1
	ror	RESULT+2
	ror	RESULT+3
	ror	FACEXTENSION
	tya			; REMAINING MULTIPLIER
	lsr	a		; LSB INTO CARRY
	bne	@1		; IF SENTINEL STILL HERE, MULTIPLY
	rts			; 8 X 32 COMPLETED


; ----------------------------------------------------------------------------
; UNPACK NUMBER AT (Y,A) INTO ARG
; ----------------------------------------------------------------------------
LOAD_ARG_FROM_YA:
	sta	INDEX		; USE INDEX FOR PNTR
	sty	INDEX+1
	ldy	#4		; FIVE BYTES TO MOVE
	lda	(INDEX),y
	sta	ARG+4
	dey
	lda	(INDEX),y
	sta	ARG+3
	dey
	lda	(INDEX),y
	sta	ARG+2
	dey
	lda	(INDEX),y
	sta	ARGSIGN
	eor	FACSIGN		; SET COMBINED SIGN FOR MULT/DIV
	sta	SGNCPR
	lda	ARGSIGN		; TURN ON NORMALIZED INVISIBLE BIT
	ora	#$80		; TO COMPLETE MANTISSA
	sta	ARG+1
	dey
	lda	(INDEX),y
	sta	ARG		; EXPONENT
	lda	FAC		; SET STATUS BITS ON FAC EXPONENT
	rts


; ----------------------------------------------------------------------------
; ADD EXPONENTS OF ARG AND FAC
; (CALLED BY FMULT AND FDIV)
;
; ALSO CHECK FOR OVERFLOW, AND SET RESULT SIGN
; ----------------------------------------------------------------------------
ADD_EXPONENTS:
	lda	ARG
; ----------------------------------------------------------------------------
ADD_EXPONENTS1:
	beq	ZERO		; IF ARG=0, RESULT IS ZERO
	clc
	adc	FAC
	bcc	@1		; IN RANGE
	bmi	JOV		; OVERFLOW
	clc
	.byte	$2C		; TRICK TO SKIP
@1:	bpl	ZERO		; OVERFLOW
	adc	#$80		; RE-BIAS
	sta	FAC		; RESULT
	beq	@3
@2:	lda	SGNCPR		; SET SIGN OF RESULT
@3:	sta	FACSIGN
	rts


; ----------------------------------------------------------------------------
; IF (FAC) IS POSITIVE, GIVE "OVERFLOW" ERROR
; IF (FAC) IS NEGATIVE, SET FAC=0, POP ONE RETURN, AND RTS
; CALLED FROM "EXP" FUNCTION
; ----------------------------------------------------------------------------
OUTOFRNG:
	lda	FACSIGN
	eor	#$FF
	bmi	JOV		; ERROR IF POSITIVE #


; ----------------------------------------------------------------------------
; POP RETURN ADDRESS AND SET FAC=0
; ----------------------------------------------------------------------------
ZERO:	pla
	pla
	jmp	ZERO_FAC
; ----------------------------------------------------------------------------
JOV:	jmp	OVERFLOW


; ----------------------------------------------------------------------------
; MULTIPLY FAC BY 10
; ----------------------------------------------------------------------------
MUL10:	jsr	COPY_FAC_TO_ARG_ROUNDED
	tax			; TEXT FAC EXPONENT
	beq	@1		; FINISHED IF FAC=0
	clc
	adc	#2		; ADD 2 TO EXPONENT GIVES (FAC)*4
	bcs	JOV		; OVERFLOW
	ldx	#0
	stx	SGNCPR
	jsr	FADD2		; MAKES (FAC)*5
	inc	FAC		; *2, MAKES (FAC)*10
	beq	JOV		; OVERFLOW
@1:	rts

; ----------------------------------------------------------------------------
CONTEN:	.byte	$84,$20,$00,$00,$00


; ----------------------------------------------------------------------------
; DIVIDE FAC BY 10
; ----------------------------------------------------------------------------
DIV10:	jsr	COPY_FAC_TO_ARG_ROUNDED
	lda	#<CONTEN	; SET UP TO PUT
	ldy	#>CONTEN	; 10 IN FAC
	ldx	#0


; ----------------------------------------------------------------------------
; FAC = ARG / (Y,A)
; ----------------------------------------------------------------------------
DIV:	stx	SGNCPR
	jsr	LOAD_FAC_FROM_YA
	jmp	FDIVT		; DIVIDE ARG BY FAC


; ----------------------------------------------------------------------------
; FAC = (Y,A) / FAC
; ----------------------------------------------------------------------------
FDIV:	jsr	LOAD_ARG_FROM_YA


; ----------------------------------------------------------------------------
; FAC = ARG / FAC
; ----------------------------------------------------------------------------
FDIVT:	beq	@8		; FAC = 0, DIVIDE BY ZERO ERROR
	jsr	ROUND_FAC
	lda	#0		; NEGATE FAC EXPONENT, SO
	sec			; ADD.EXPONENTS FORMS DIFFERENCE
	sbc	FAC
	sta	FAC
	jsr	ADD_EXPONENTS
	inc	FAC
	beq	JOV		; OVERFLOW
	ldx	#-4		; INDEX FOR RESULT
	lda	#1		; SENTINEL
@1:	ldy	ARG+1		; SEE IF FAC CAN BE SUBTRACTED
	cpy	FAC+1
	bne	@2
	ldy	ARG+2
	cpy	FAC+2
	bne	@2
	ldy	ARG+3
	cpy	FAC+3
	bne	@2
	ldy	ARG+4
	cpy	FAC+4
@2:	php			; SAVE THE ANSWER, AND ALSO ROLL THE
	rol	a		; BIT INTO THE QUOTIENT, SENTINEL OUT
	bcc	@3		; NO SENTINEL, STILL NOT 8 TRIPS
	inx			; 8 TRIPS, STORE BYTE OF QUOTIENT
	sta	RESULT+3,x
	beq	@6		; 32-BITS COMPLETED
	bpl	@7		; FINAL EXIT WHEN X=1
	lda	#1		; RE-START SENTINEL
@3:	plp			; GET ANSWER, CAN FAC BE SUBTRACTED?
	bcs	@5		; YES, DO IT
@4:	asl	ARG+4		; NO, SHIFT ARG LEFT
	rol	ARG+3
	rol	ARG+2
	rol	ARG+1
	bcs	@2		; ANOTHER TRIP
	bmi	@1		; HAVE TO COMPARE FIRST
	bpl	@2		; ...ALWAYS
@5:	tay			; SAVE QUOTIENT/SENTINEL BYTE
	lda	ARG+4		; SUBTRACT FAC FROM ARG ONCE
	sbc	FAC+4
	sta	ARG+4
	lda	ARG+3
	sbc	FAC+3
	sta	ARG+3
	lda	ARG+2
	sbc	FAC+2
	sta	ARG+2
	lda	ARG+1
	sbc	FAC+1
	sta	ARG+1
	tya			; RESTORE QUOTIENT/SENTINEL BYTE
	jmp	@4		; GO TO SHIFT ARG AND CONTINUE
; ----------------------------------------------------------------------------
@6:	lda	#$40		; DO A FEW EXTENSION BITS
	bne	@3		; ...ALWAYS
; ----------------------------------------------------------------------------
@7:	asl	a		; LEFT JUSTIFY THE EXTENSION BITS WE DID
	asl	a
	asl	a
	asl	a
	asl	a
	asl	a
	sta	FACEXTENSION
	plp
	jmp	COPY_RESULT_INTO_FAC
; ----------------------------------------------------------------------------
@8:	ldx	#ERR_ZERODIV
	jmp	ERROR


; ----------------------------------------------------------------------------
; COPY RESULT INTO FAC MANTISSA, AND NORMALIZE
; ----------------------------------------------------------------------------
COPY_RESULT_INTO_FAC:
	lda	RESULT
	sta	FAC+1
	lda	RESULT+1
	sta	FAC+2
	lda	RESULT+2
	sta	FAC+3
	lda	RESULT+3
	sta	FAC+4
	jmp	NORMALIZE_FAC2


; ----------------------------------------------------------------------------
; UNPACK (Y,A) INTO FAC
; ----------------------------------------------------------------------------
LOAD_FAC_FROM_YA:
	sta	INDEX		; USE INDEX FOR PNTR
	sty	INDEX+1
	ldy	#4		; PICK UP 5 BYTES
	lda	(INDEX),y
	sta	FAC+4
	dey
	lda	(INDEX),y
	sta	FAC+3
	dey
	lda	(INDEX),y
	sta	FAC+2
	dey
	lda	(INDEX),y
	sta	FACSIGN		; FIRST BIT IS SIGN
	ora	#$80		; SET NORMALIZED INVISIBLE BIT
	sta	FAC+1
	dey
	lda	(INDEX),y
	sta	FAC		; EXPONENT
	sty	FACEXTENSION	; Y=0
	rts


; ----------------------------------------------------------------------------
; ROUND FAC, STORE IN TEMP2
; ----------------------------------------------------------------------------
STORE_FAC_IN_TEMP2_ROUNDED:
	ldx	#TEMP2		; PACK FAC INTO TEMP2
	.byte	$2C		; TRICK TO BRANCH


; ----------------------------------------------------------------------------
; ROUND FAC, STORE IN TEMP1
; ----------------------------------------------------------------------------
STORE_FAC_IN_TEMP1_ROUNDED:
	ldx	#<TEMP1		; PACK FAC INTO TEMP1
	ldy	#>TEMP1		; HI-BYTE OF TEMP1 SAME AS TEMP2
	beq	STORE_FAC_AT_YX_ROUNDED	; ...ALWAYS


; ----------------------------------------------------------------------------
; ROUND FAC, AND STORE WHERE FORPNT POINTS
; ----------------------------------------------------------------------------
SETFOR:	ldx	FORPNT
	ldy	FORPNT+1


; ----------------------------------------------------------------------------
; ROUND FAC, AND STORE AT (Y,X)
; ----------------------------------------------------------------------------
STORE_FAC_AT_YX_ROUNDED:
	jsr	ROUND_FAC	; ROUND VALUE IN FAC USING EXTENSION
	stx	INDEX		; USE INDEX FOR PNTR
	sty	INDEX+1
	ldy	#4		; STORING 5 PACKED BYTES
	lda	FAC+4
	sta	(INDEX),y
	dey
	lda	FAC+3
	sta	(INDEX),y
	dey
	lda	FAC+2
	sta	(INDEX),y
	dey
	lda	FACSIGN		; PACK SIGN IN TOP BIT OF MANTISSA
	ora	#$7F
	and	FAC+1
	sta	(INDEX),y
	dey
	lda	FAC		; EXPONENT
	sta	(INDEX),y
	sty	FACEXTENSION	; ZERO THE EXTENSION
	rts


; ----------------------------------------------------------------------------
; COPY ARG INTO FAC
; ----------------------------------------------------------------------------
COPY_ARG_TO_FAC:
	lda	ARGSIGN		; COPY SIGN
MFA:	sta	FACSIGN
	ldx	#5		; MOVE 5 BYTES
@1:	lda	ARG-1,x
	sta	FAC-1,x
	dex
	bne	@1
	stx	FACEXTENSION	; ZERO EXTENSION
	rts


; ----------------------------------------------------------------------------
; ROUND FAC AND COPY TO ARG
; ----------------------------------------------------------------------------
COPY_FAC_TO_ARG_ROUNDED:
	jsr	ROUND_FAC	; ROUND FAC USING EXTENSION
MAF:	ldx	#6		; COPY 6 BYTES, INCLUDES SIGN
@1:	lda	FAC-1,x
	sta	ARG-1,x
	dex
	bne	@1
	stx	FACEXTENSION	; ZERO FAC EXTENSION
RTS14:	rts


; ----------------------------------------------------------------------------
; ROUND FAC USING EXTENSION BYTE
; ----------------------------------------------------------------------------
ROUND_FAC:
	lda	FAC
	beq	RTS14		; FAC = 0, RETURN
	asl	FACEXTENSION	; IS FAC.EXTENSION >= 128?
	bcc	RTS14		; NO, FINISHED


; ----------------------------------------------------------------------------
; INCREMENT MANTISSA AND RE-NORMALIZE IF CARRY
; ----------------------------------------------------------------------------
INCREMENT_MANTISSA:
	jsr	INCREMENT_FAC_MANTISSA	; YES, INCREMENT FAC
	bne	RTS14		; HIGH BYTE HAS BITS, FINISHED
	jmp	NORMALIZE_FAC6	; HI-BYTE=0, SO SHIFT LEFT


; ----------------------------------------------------------------------------
; TEST FAC FOR ZERO AND SIGN
;
; FAC > 0, RETURN +1
; FAC = 0, RETURN  0
; FAC < 0, RETURN -1
; ----------------------------------------------------------------------------
SIGN:	lda	FAC		; CHECK SIGN OF FAC AND
	beq	RTS15		; RETURN -1,0,1 IN A-REG
; ----------------------------------------------------------------------------
SIGN1:	lda	FACSIGN
; ----------------------------------------------------------------------------
SIGN2:	rol	a		; MSBIT TO CARRY
	lda	#$FF		; -1
	bcs	RTS15		; MSBIT = 1
	lda	#1		; +1
RTS15:	rts


; ----------------------------------------------------------------------------
; "SGN" FUNCTION
; ----------------------------------------------------------------------------
SGN:	jsr	SIGN		; CONVERT FAC TO -1,0,1


; ----------------------------------------------------------------------------
; CONVERT (A) INTO FAC, AS SIGNED VALUE -128 TO +127
; ----------------------------------------------------------------------------
FLOAT:	sta	FAC+1		; PUT IN HIGH BYTE OF MANTISSA
	lda	#0		; CLEAR 2ND BYTE OF MANTISSA
	sta	FAC+2
	ldx	#$88		; USE EXPONENT 2^9


; ----------------------------------------------------------------------------
; FLOAT UNSIGNED VALUE IN FAC+1,2
; (X) = EXPONENT
; ----------------------------------------------------------------------------
FLOAT1:	lda	FAC+1		; MSBIT=0, SET CARRY; =1, CLEAR CARRY
	eor	#$FF
	rol	a


; ----------------------------------------------------------------------------
; FLOAT UNSIGNED VALUE IN FAC+1,2
; (X) = EXPONENT
; C=0 TO MAKE VALUE NEGATIVE
; C=1 TO MAKE VALUE POSITIVE
; ----------------------------------------------------------------------------
FLOAT2:	lda	#0		; CLEAR LOWER 16-BITS OF MANTISSA
	sta	FAC+4
	sta	FAC+3
	stx	FAC		; STORE EXPONENT
	sta	FACEXTENSION	; CLEAR EXTENSION
	sta	FACSIGN		; MAKE SIGN POSITIVE
	jmp	NORMALIZE_FAC1	; IF C=0, WILL NEGATE FAC


; ----------------------------------------------------------------------------
; "ABS" FUNCTION
; ----------------------------------------------------------------------------
ABS:	lsr	FACSIGN		; CHANGE SIGN TO +
	rts


; ----------------------------------------------------------------------------
; COMPARE FAC WITH PACKED # AT (Y,A)
; RETURN A=1,0,-1 AS (Y,A) IS <,=,> FAC
; ----------------------------------------------------------------------------
FCOMP:	sta	DEST		; USE DEST FOR PNTR


; ----------------------------------------------------------------------------
; SPECIAL ENTRY FROM "NEXT" PROCESSOR
; "DEST" ALREADY SET UP
; ----------------------------------------------------------------------------
FCOMP2:	sty	DEST+1
	ldy	#0		; GET EXPONENT OF COMPARAND
	lda	(DEST),y
	iny			; POINT AT NEXT BYTE
	tax			; EXPONENT TO X-REG
	beq	SIGN		; IF COMPARAND=0, "SIGN" COMPARES FAC
	lda	(DEST),y	; GET HI-BYTE OF MANTISSA
	eor	FACSIGN		; COMPARE WITH FAC SIGN
	bmi	SIGN1		; DIFFERENT SIGNS, "SIGN" GIVES ANSWER
	cpx	FAC		; SAME SIGN, SO COMPARE EXPONENTS
	bne	@1		; DIFFERENT, SO SUFFICIENT TEST
	lda	(DEST),y	; SAME EXPONENT, COMPARE MANTISSA
	ora	#$80		; SET INVISIBLE NORMALIZED BIT
	cmp	FAC+1
	bne	@1		; NOT SAME, SO SUFFICIENT
	iny			; SAME, COMPARE MORE MANTISSA
	lda	(DEST),y
	cmp	FAC+2
	bne	@1		; NOT SAME, SO SUFFICIENT
	iny			; SAME, COMPARE MORE MANTISSA
	lda	(DEST),y
	cmp	FAC+3
	bne	@1		; NOT SAME, SO SUFFICIENT
	iny			; SAME, COMPARE REST OF MANTISSA
	lda	#$7F		; ARTIFICIAL EXTENSION BYTE FOR COMPARAND
	cmp	FACEXTENSION
	lda	(DEST),y
	sbc	FAC+4
	beq	RTS16		; NUMBERS ARE EQUAL, RETURN (A)=0
@1:	ror			; PUT CARRY INTO SIGN BIT
	eor	FACSIGN		; TOGGLE WITH SIGN OF FAC
@2:	jmp	SIGN2		; CONVERT +1 OR -1


; ----------------------------------------------------------------------------
; QUICK INTEGER FUNCTION
;
; CONVERTS FP VALUE IN FAC TO INTEGER VALUE
; IN FAC+1...FAC+4, BY SHIFTING RIGHT WITH SIGN
; EXTENSION UNTIL FRACTIONAL BITS ARE OUT.
;
; THIS SUBROUTINE ASSUMES THE EXPONENT < 32.
; ----------------------------------------------------------------------------
QINT:	lda	FAC		; LOOK AT FAC EXPONENT
	beq	QINT3		; FAC=0, SO FINISHED
	sec			; GET -(NUMBER OF FRACTIONAL BITS)
	sbc	#$A0		; IN A-REG FOR SHIFT COUNT
	bit	FACSIGN		; CHECK SIGN OF FAC
	bpl	@1		; POSITIVE, CONTINUE
	tax			; NEGATIVE, SO COMPLEMENT MANTISSA
	lda	#$FF		; AND SET SIGN EXTENSION FOR SHIFT
	sta	SHIFTSIGNEXT
	jsr	COMPLEMENT_FAC_MANTISSA
	txa			; RESTORE BIT COUNT TO A-REG
@1:	ldx	#FAC		; POINT SHIFT SUBROUTINE AT FAC
	cmp	#$F9		; MORE THAN 7 BITS TO SHIFT?
	bpl	QINT2		; NO, SHORT SHIFT
	jsr	SHIFT_RIGHT	; YES, USE GENERAL ROUTINE
	sty	SHIFTSIGNEXT	; Y=0, CLEAR SIGN EXTENSION
RTS16:	rts
; ----------------------------------------------------------------------------
QINT2:	tay			; SAVE SHIFT COUNT
	lda	FACSIGN		; GET SIGN BIT
	and	#$80
	lsr	FAC+1		; START RIGHT SHIFT
	ora	FAC+1		; AND MERGE WITH SIGN
	sta	FAC+1
	jsr	SHIFT_RIGHT4	; JUMP INTO MIDDLE OF SHIFTER
	sty	SHIFTSIGNEXT	; Y=0, CLEAR SIGN EXTENSION
	rts


; ----------------------------------------------------------------------------
; "INT" FUNCTION
;
; USES QINT TO CONVERT (FAC) TO INTEGER FORM,
; AND THEN REFLOATS THE INTEGER.
; ----------------------------------------------------------------------------
INT:	lda	FAC		; CHECK IF EXPONENT < 32
	cmp	#$A0		; BECAUSE IF > 31 THERE IS NO FRACTION
	bcs	RTS17		; NO FRACTION, WE ARE FINISHED
	jsr	QINT		; USE GENERAL INTEGER CONVERSION
	sty	FACEXTENSION	; Y=0, CLEAR EXTENSION
	lda	FACSIGN		; GET SIGN OF VALUE
	sty	FACSIGN		; Y=0, CLEAR SIGN
	eor	#$80		; TOGGLE ACTUAL SIGN
	rol	a		; AND SAVE IN CARRY
	lda	#$A0		; SET EXPONENT TO 32
	sta	FAC		; BECAUSE 4-BYTE INTEGER NOW
	lda	FAC+4		; SAVE LOW 8-BITS OF INTEGER FORM
	sta	CHARAC		; FOR EXP AND POWER
	jmp	NORMALIZE_FAC1	; NORMALIZE TO FINISH CONVERSION
; ----------------------------------------------------------------------------
QINT3:	sta	FAC+1		; FAC=0, SO CLEAR ALL 4 BYTES FOR
	sta	FAC+2		; INTEGER VERSION
	sta	FAC+3
	sta	FAC+4
	tay			; Y=0 TOO
RTS17:	rts


; ----------------------------------------------------------------------------
; CONVERT STRING TO FP VALUE IN FAC
;
; STRING POINTED TO BY TXTPTR
; FIRST CHAR ALREADY SCANNED BY CHRGET
; (A) = FIRST CHAR, C=0 IF DIGIT.
; ----------------------------------------------------------------------------
FIN:    ldy	#0		; CLEAR WORKING AREA ($99...$A3)
	ldx	#10		; TMPEXP, EXPON, DPFLG, EXPSGN, FAC, SERLEN
@1:	sty	TMPEXP,x
	dex
	bpl	@1
; ----------------------------------------------------------------------------
	bcc	FIN2		; FIRST CHAR IS A DIGIT
	cmp	#'-'		; CHECK FOR LEADING SIGN
	bne	@2		; NOT MINUS
	stx	SERLEN		; MINUS, SET SERLEN = $FF FOR FLAG
	beq	FIN1		; ...ALWAYS
@2:	cmp	#'+'		; MIGHT BE PLUS
	bne	FIN3		; NOT PLUS EITHER, CHECK DECIMAL POINT

; ----------------------------------------------------------------------------
FIN1:	jsr	CHRGET		; GET NEXT CHAR OF STRING

; ----------------------------------------------------------------------------
FIN2:	bcc	FIN9		; INSERT THIS DIGIT

; ----------------------------------------------------------------------------
FIN3:	cmp	#'.'		; CHECK FOR DECIMAL POINT
	beq	FIN10		; YES
	cmp	#'E'		; CHECK FOR EXPONENT PART
	bne	FIN7		; NO, END OF NUMBER
	jsr	CHRGET		; YES, START CONVERTING EXPONENT
	bcc	FIN5		; EXPONENT DIGIT
	cmp	#TOKEN_MINUS	; NEGATIVE EXPONENT?
	beq	@1		; YES
	cmp	#'-'		; MIGHT NOT BE TOKENIZED YET
	beq	@1		; YES, IT IS NEGATIVE
	cmp	#TOKEN_PLUS	; OPTIONAL "+"
	beq	FIN4		; YES
	cmp	#'+'		; MIGHT NOT BE TOKENIZED YET
	beq	FIN4		; YES, FOUND "+"
	bne	FIN6		; ...ALWAYS, NUMBER COMPLETED
@1:	ror     EXPSGN		; C=1, SET FLAG NEGATIVE

; ----------------------------------------------------------------------------
FIN4:	jsr	CHRGET		; GET NEXT DIGIT OF EXPONENT

; ----------------------------------------------------------------------------
FIN5:	bcc	GETEXP		; CHAR IS A DIGIT OF EXPONENT

; ----------------------------------------------------------------------------
FIN6:	bit	EXPSGN		; END OF NUMBER, CHECK EXP SIGN
	bpl	FIN7		; POSITIVE EXPONENT
	lda	#0		; NEGATIVE EXPONENT
	sec			; MAKE 2'S COMPLEMENT OF EXPONENT
	sbc	EXPON
	jmp	FIN8
; ----------------------------------------------------------------------------
; FOUND A DECIMAL POINT
; ----------------------------------------------------------------------------
FIN10:	ror	DPFLG		; C=1, SET DPFLG FOR DECIMAL POINT
	bit	DPFLG		; CHECK IF PREVIOUS DEC. PT.
	bvc	FIN1		; NO PREVIOUS DECIMAL POINT
; A SECOND DECIMAL POINT IS TAKEN AS A TERMINATOR
; TO THE NUMERIC STRING.
; "A=11..22" WILL GIVE A SYNTAX ERROR, BECAUSE
; IT IS TWO NUMBERS WITH NO OPERATOR BETWEEN.
; "PRINT 11..22" GIVES NO ERROR, BECAUSE IT IS
; JUST THE CONCATENATION OF TWO NUMBERS.

; ----------------------------------------------------------------------------
; NUMBER TERMINATED, ADJUST EXPONENT NOW
; ----------------------------------------------------------------------------
FIN7:	lda	EXPON		; E-VALUE
FIN8:	sec			; MODIFY WITH COUNT OF DIGITS
	sbc	TMPEXP		; AFTER THE DECIMAL POINT
	sta	EXPON		; COMPLETE CURRENT EXPONENT
	beq	@15		; NO ADJUST NEEDED IF EXP=0
	bpl	@14		; EXP>0, MULTIPLY BY TEN
@13:	jsr	DIV10		; EXP<0, DIVIDE BY TEN
	inc	EXPON		; UNTIL EXP=0
	bne	@13
	beq	@15		; ...ALWAYS, WE ARE FINISHED
@14:	jsr	MUL10		; EXP>0, MULTIPLY BKY TEN
	dec	EXPON		; UNTIL EXP=0
	bne	@14
@15:	lda	SERLEN		; IS WHOLE NUMBER NEGATIVE?
	bmi	@16		; YES
	rts			; NO, RETURN, WHOLE JOB DONE!
@16:	jmp	NEGOP		; NEGATIVE NUMBER, SO NEGATE FAC


; ----------------------------------------------------------------------------
; ACCUMULATE A DIGIT INTO FAC
; ----------------------------------------------------------------------------
FIN9:	pha			; SAVE DIGIT
	bit	DPFLG		; SEEN A DECIMAL POINT YET?
	bpl	@1		; NO, STILL IN INTEGER PART
	inc	TMPEXP		; YES, COUNT THE FRACTIONAL DIGIT
@1:	jsr	MUL10		; FAC = FAC * 10
	pla			; CURRENT DIGIT
	and	#$0F
	jsr	ADDACC		; ADD THE DIGIT
	jmp	FIN1		; GO BACK FOR MORE


; ----------------------------------------------------------------------------
; ADD (A) TO FAC
; ----------------------------------------------------------------------------
ADDACC:	pha			; SAVE ADDEND
	jsr	COPY_FAC_TO_ARG_ROUNDED
	pla			; GET ADDEND AGAIN
	jsr	FLOAT		; CONVERT TO FP VALUE IN FAC
	lda	ARGSIGN
	eor	FACSIGN
	sta	SGNCPR
	ldx	FAC		; TO SIGNAL IF FAC=0
	jmp	FADDT		; PERFORM THE ADDITION


; ----------------------------------------------------------------------------
; ACCUMULATE DIGIT OF EXPONENT
; ----------------------------------------------------------------------------
GETEXP:	lda	EXPON		; CHECK CURRENT VALUE
	cmp	#10		; FOR MORE THAN 2 DIGITS
	bcc	@1		; NO, THIS IS 1ST OR 2ND DIGIT
	lda	#100		; EXPONENT TOO BIG
	bit	EXPSGN		; UNLESS IT IS NEGATIVE
	bmi	@2		; LARGE NEGATIVE EXPONENT MAKES FAC=0
	jmp	OVERFLOW	; LARGE POSITIVE EXPONENT IS ERROR
@1:	asl	a		; EXPONENT TIMES 10
	asl	a
	clc
	adc	EXPON
	asl	a
	ldy	#0		; ADD THE NEW DIGIT
	adc	(TXTPTR),y	; BUT THIS IS IN ASCII,
	sec			; SO ADJUST BACK TO BINARY
	sbc	#'0'
@2:	sta	EXPON		; NEW VALUE
	jmp	FIN4		; BACK FOR MORE

; ----------------------------------------------------------------------------
CON_99999999_9:
	.byte	$9B,$3E,$BC,$1F,$FD	; 99,999,999.9

CON_999999999:
	.byte	$9E,$6E,$6B,$27,$FD	; 999,999,999

CON_BILLION:
	.byte	$9E,$6E,$6B,$28,$00	; 1,000,000,000


; ----------------------------------------------------------------------------
; PRINT "IN <LINE #>"
; ----------------------------------------------------------------------------
INPRT:	lda	#<QT_IN		; PRINT " IN "
	ldy	#>QT_IN
	jsr	STROUT
	lda	CURLIN+1
	ldx	CURLIN


; ----------------------------------------------------------------------------
; PRINT A,X AS DECIMAL INTEGER
; ----------------------------------------------------------------------------
LINPRT:	sta	FAC+1		; PRINT A,X IN DECIMAL
	stx	FAC+2
	ldx	#$90		; EXPONENT = 2^16
	sec			; CONVERT UNSIGNED
	jsr	FLOAT2		; CONVERT LINE # TO FP
	jsr	FOUT		; CONVERT (FAC) TO STRING AT STACK
	jmp	STROUT		; PRINT STRING AT A,Y


; ----------------------------------------------------------------------------
; CONVERT (FAC) TO STRING STARTING AT STACK
; RETURN WITH (Y,A) POINTING AT STRING
; ----------------------------------------------------------------------------
FOUT:	ldy	#1		; NORMAL ENTRY PUTS STRING AT STACK...


; ----------------------------------------------------------------------------
; "STR$" FUNCTION ENTERS HERE, WITH (Y)=0
; SO THAT RESULT STRING STARTS AT STACK-1
; (THIS IS USED AS A FLAG)
; ----------------------------------------------------------------------------
FOUT1:	lda	#'-'		; IN CASE VALUE NEGATIVE
	dey			; BACK UP PNTR
	bit	FACSIGN
	bpl	@1		; VALUE IS +
	iny			; VALUE IS -
	sta	STACK-1,y	; EMIT "-"
@1:	sta	FACSIGN		; MAKE FAC.SIGN POSITIVE ($2D)
	sty	STRNG2		; SAVE STRING PNTR
	iny
	lda	#'0'		; IN CASE (FAC)=0
	ldx	FAC		; NUMBER=0?
	bne	@2		; NO, (FAC) NOT ZERO
	jmp	FOUT4		; YES, FINISHED
; ----------------------------------------------------------------------------
@2:	lda	#0		; STARTING VALUE FOR TMPEXP
	cpx	#$80		; ANY INTEGER PART?
	beq	@3		; NO, BTWN .5 AND .999999999
	bcs	@4		; YES
; ----------------------------------------------------------------------------
@3:	lda	#<CON_BILLION	; MULTIPLY BY 1E9
	ldy	#>CON_BILLION	; TO GIVE ADJUSTMENT A HEAD START
	jsr	FMULT
	lda	#-9		; EXPONENT ADJUSTMENT
@4:	sta	TMPEXP		; 0 OR -9
; ----------------------------------------------------------------------------
; ADJUST UNTIL 1E8 <= (FAC) <1E9
; ----------------------------------------------------------------------------
@5:	lda	#<CON_999999999
	ldy	#>CON_999999999
	jsr	FCOMP		; COMPARE TO 1E9-1
	beq	@10		; (FAC) = 1E9-1
	bpl	@8		; TOO LARGE, DIVIDE BY TEN
@6:	lda	#<CON_99999999_9	; COMPARE TO 1E8-.1
	ldy	#>CON_99999999_9
	jsr	FCOMP		; COMPARE TO 1E8-.1
	beq	@7		; (FAC) = 1E8-.1
	bpl	@9		; IN RANGE, ADJUSTMENT FINISHED
@7:	jsr	MUL10		; TOO SMALL, MULTIPLY BY TEN
	dec	TMPEXP		; KEEP TRACK OF MULTIPLIES
	bne	@6		; ...ALWAYS
@8:	jsr	DIV10		; TOO LARGE, DIVIDE BY TEN
	inc	TMPEXP		; KEEP TRACK OF DIVISIONS
	bne	@5		; ...ALWAYS
; ----------------------------------------------------------------------------
@9:	jsr	FADDH		; ROUND ADJUSTED RESULT
@10:	jsr	QINT		; CONVERT ADJUSTED VALUE TO 32-BIT INTEGER


; ----------------------------------------------------------------------------
; FAC+1...FAC+4 IS NOW IN INTEGER FORM
; WITH POWER OF TEN ADJUSTMENT IN TMPEXP
;
; IF -10 < TMPEXP > 1, PRINT IN DECIMAL FORM
; OTHERWISE, PRINT IN EXPONENTIAL FORM
; ----------------------------------------------------------------------------
FOUT2:	ldx	#1		; ASSUME 1 DIGIT BEFORE "."
	lda	TMPEXP		; CHECK RANGE
	clc
	adc	#10
	bmi	@1		; < .01, USE EXPONENTIAL FORM
	cmp	#11
	bcs	@2		; >= 1E10, USE EXPONENTIAL FORM
	adc	#$FF		; LESS 1 GIVES INDEX FOR "."
	tax
	lda	#2		; SET REMAINING EXPONENT = 0
@1:	sec			; COMPUTE REMAINING EXPONENT
@2:	sbc	#2
	sta	EXPON		; VALUE FOR "E+XX" OR "E-XX"
	stx	TMPEXP		; INDEX FOR DECIMAL POINT
	txa			; SEE IF "." COMES FIRST
	beq	@3		; YES
	bpl	@5		; NO, LATER
@3:	ldy	STRNG2		; GET INDEX INTO STRING BEING BUILT
	lda	#'.'		; STORE A DECIMAL POINT
	iny
	sta	STACK-1,y
	txa			; SEE IF NEED ".0"
	beq	@4		; NO
	lda	#'0'		; YES, STORE "0"
	iny
	sta	STACK-1,y
@4:	sty	STRNG2		; SAVE OUTPUT INDEX AGAIN
; ----------------------------------------------------------------------------
; NOW DIVIDE BY POWERS OF TEN TO GET SUCCESSIVE DIGITS
; ----------------------------------------------------------------------------
@5:	ldy	#0		; INDEX TO TABLE OF POWERS OF TEN
	ldx	#$80		; STARTING VALUE FOR DIGIT WITH DIRECTION
@6:	lda	FAC+4		; START BY ADDING -100000000 UNTIL
	clc			; OVERSHOOT.  THEN ADD +10000000,
	adc	DECTBL+3,y	; THEN ADD -1000000, THEN ADD
	sta	FAC+4		; +100000, AND SO ON.
	lda	FAC+3		; THE # OF TIMES EACH POWER IS ADDED
	adc	DECTBL+2,y	; IS 1 MORE THAN CORRESPONDING DIGIT
	sta	FAC+3
	lda	FAC+2
	adc	DECTBL+1,y
	sta	FAC+2
	lda	FAC+1
	adc	DECTBL,y
	sta	FAC+1
	inx			; COUNT THE ADD
	bcs	@7		; IF C=1 AND X NEGATIVE, KEEP ADDING
	bpl	@6		; IF C=0 AND X POSITIVE, KEEP ADDING
	bmi	@8		; IF C=0 AND X NEGATIVE, WE OVERSHOT
@7:	bmi	@6		; IF C=1 AND X POSITIVE, WE OVERSHOT
@8:	txa			; OVERSHOT, SO MAKE X INTO A DIGIT
	bcc	@9		; HOW DEPENDS ON DIRECTION WE WERE GOING
	eor	#$FF		; DIGIT = 9-X
	adc	#10
@9:	adc	#'0'-1		; MAKE DIGIT INTO ASCII
	iny			; ADVANCE TO NEXT SMALLER POWER OF TEN
	iny
	iny
	iny
	sty	VARPNT		; SAVE PNTR TO POWERS
	ldy	STRNG2		; GET OUTPUT PNTR
	iny			; STORE THE DIGIT
	tax			; SAVE DIGIT, HI-BIT IS DIRECTION
	and	#$7F		; MAKE SURE $30...$39 FOR STRING
	sta	STACK-1,y
	dec	TMPEXP		; COUNT THE DIGIT
	bne	@10		; NOT TIME FOR "." YET
	lda	#'.'		; TIME, SO STORE THE DECIMAL POINT
	iny
	sta	STACK-1,y
@10:	sty	STRNG2		; SAVE OUTPUT PNTR AGAIN
	ldy	VARPNT		; GET PNTR TO POWERS
	txa			; GET DIGIT WITH HI-BIT = DIRECTION
	eor	#$FF		; CHANGE DIRECTION
	and	#$80		; $00 IF ADDING, $80 IF SUBTRACTING
	tax
	cpy	#DECTBL_END-DECTBL
	bne	@6		; NOT FINISHED YET


; ----------------------------------------------------------------------------
; NINE DIGITS HAVE BEEN STORED IN STRING.  NOW LOOK
; BACK AND LOP OFF TRAILING ZEROES AND A TRAILING
; DECIMAL POINT.
; ----------------------------------------------------------------------------
FOUT3:	ldy	STRNG2		; POINTS AT LAST STORED CHAR
@1:	lda	STACK-1,y	; SEE IF LOPPABLE
	dey
	cmp	#'0'		; SUPPRESS TRAILING ZEROES
	beq	@1		; YES, KEEP LOOPING
	cmp	#'.'		; SUPPRESS TRAILING DECIMAL POINT
	beq	@2		; ".", SO WRITE OVER IT
	iny			; NOT ".", SO INCLUDE IN STRING AGAIN
@2:	lda	#'+'		; PREPARE FOR POSITIVE EXPONENT "E+XX"
	ldx	EXPON		; SEE IF ANY E-VALUE
	beq	FOUT5		; NO, JUST MARK END OF STRING
	bpl	@3		; YES, AND IT IS POSITIVE
	lda	#0		; YES, AND IT IS NEGATIVE
	sec			; COMPLEMENT THE VALUE
	sbc	EXPON
	tax			; GET MAGNITUDE IN X
	lda	#'-'		; E SIGN
@3:	sta	STACK+1,y	; STORE SIGN IN STRING
	lda	#'E'		; STORE "E" IN STRING BEFORE SIGN
	sta	STACK,y
	txa			; EXPONENT MAGNITUDE IN A-REG
	ldx	#'0'-1		; SEED FOR EXPONENT DIGIT
	sec			; CONVERT TO DECIMAL
@4:	inx			; COUNT THE SUBTRACTION
	sbc	#10		; TEN'S DIGIT
	bcs	@4		; MORE TENS TO SUBTRACT
	adc	#'0'+10		; CONVERT REMAINDER TO ONE'S DIGIT
	sta	STACK+3,y	; STORE ONE'S DIGIT
	txa
	sta	STACK+2,y	; STORE TEN'S DIGIT
	lda	#0		; MARK END OF STRING WITH $00
	sta	STACK+4,y
	beq	FOUT6		; ...ALWAYS
FOUT4:	sta	STACK-1,y	; STORE "0" IN ASCII
FOUT5:	lda	#0		; STORE $00 ON END OF STRING
	sta	STACK,y
FOUT6:	lda	#<STACK		; POINT Y,A AT BEGINNING OF STRING
	ldy	#>STACK		; (STR$ STARTED STRING AT STACK-1, BUT
	rts			; STR$ DOESN'T USE Y,A ANYWAY.)


; ----------------------------------------------------------------------------
CON_HALF:
	.byte	$80,$00,$00,$00,$00	; FP CONSTANT 0.5


; ----------------------------------------------------------------------------
; POWERS OF 10 FROM 1E8 DOWN TO 1,
; AS 32-BIT INTEGERS, WITH ALTERNATING SIGNS
; ----------------------------------------------------------------------------
DECTBL: .byte	$FA,$0A,$1F,$00		; -100000000
	.byte	$00,$98,$96,$80		; 10000000
	.byte	$FF,$F0,$BD,$C0		; -1000000
	.byte	$00,$01,$86,$A0		; 100000
	.byte	$FF,$FF,$D8,$F0		; -10000
	.byte   $00,$00,$03,$E8		; 1000
	.byte	$FF,$FF,$FF,$9C		; -100
	.byte   $00,$00,$00,$0A		; 10
	.byte	$FF,$FF,$FF,$FF		; -1
DECTBL_END:


; ----------------------------------------------------------------------------
; "SQR" FUNCTION
; ----------------------------------------------------------------------------
SQR:	jsr	COPY_FAC_TO_ARG_ROUNDED
	lda	#<CON_HALF		; SET UP POWER OF 0.5
	ldy	#>CON_HALF
	jsr	LOAD_FAC_FROM_YA


; ----------------------------------------------------------------------------
; EXPONENTIATION OPERATION
;
; ARG ^ FAC  =  EXP( LOG(ARG) * FAC )
; ----------------------------------------------------------------------------
FPWRT:	beq	EXP		; IF FAC=0, ARG^FAC=EXP(0)
	lda	ARG		; IF ARG=0, ARG^FAC=0
	bne	@1		; NEITHER IS ZERO
	jmp	STA_IN_FAC_SIGN_AND_EXP		; SET FAC = 0
@1:	ldx	#TEMP3		; SAVE FAC IN TEMP3
	ldy	#0
	jsr	STORE_FAC_AT_YX_ROUNDED
	lda	ARGSIGN		; NORMALLY, ARG MUST BE POSITIVE
	bpl	@2		; IT IS POSITIVE, SO ALL IS WELL
	jsr	INT		; NEGATIVE, BUT OK IF INTEGRAL POWER
	lda	#TEMP3		; SEE IF INT(FAC)=FAC
	ldy	#0
	jsr	FCOMP		; IS IT AN INTEGER POWER?
	bne	@2		; NOT INTEGRAL,  WILL CAUSE ERROR LATER
	tya			; MAKE ARG SIGN + AS IT IS MOVED TO FAC
	ldy	CHARAC		; INTEGRAL, SO ALLOW NEGATIVE ARG
@2:	jsr	MFA		; MOVE ARGUMENT TO FAC
	tya			; SAVE FLAG FOR NEGATIVE ARG (0=+) 
	pha
	jsr	LOG		; GET LOG(ARG)
	lda	#TEMP3		; MULTIPLY BY POWER
	ldy	#0
	jsr	FMULT
	jsr	EXP		; E ^ LOG(FAC)
	pla			; GET FLAG FOR NEGATIVE ARG
	bpl	RTS18		; NOT NEGATIVE, FINISHED


; ----------------------------------------------------------------------------
; NEGATE VALUE IN FAC
; ----------------------------------------------------------------------------
NEGOP:	lda	FAC		; IF FAC=0, NO NEED TO COMPLEMENT
	beq	RTS18		; YES, FAC=0
	lda	FACSIGN		; NO, SO TOGGLE SIGN
	eor	#$FF
	sta	FACSIGN
RTS18:	rts


; ----------------------------------------------------------------------------
CON_LOG_E:
	.byte	$81,$38,$AA,$3B,$29	; LOG(E) TO BASE 2
; ----------------------------------------------------------------------------
POLY_EXP:
	.byte	$07			; ( # OF TERMS IN POLYNOMIAL) - 1
	.byte	$71,$34,$58,$3E,$56	; (LOG(2)^7)/8!
	.byte	$74,$16,$7E,$B3,$1B	; (LOG(2)^6)/7!
	.byte	$77,$2F,$EE,$E3,$85	; (LOG(2)^5)/6!
	.byte   $7A,$1D,$84,$1C,$2A	; (LOG(2)^4)/5!
	.byte	$7C,$63,$59,$58,$0A	; (LOG(2)^3)/4!
	.byte	$7E,$75,$FD,$E7,$C6	; (LOG(2)^2)/3!
	.byte	$80,$31,$72,$18,$10	; LOG(2)/2!
	.byte	$81,$00,$00,$00,$00	; 1


; ----------------------------------------------------------------------------
; "EXP" FUNCTION
;
; FAC = E ^ FAC
; ----------------------------------------------------------------------------
EXP:	lda	#<CON_LOG_E	; CONVERT TO POWER OF TWO PROBLEM
	ldy	#>CON_LOG_E	; E^X = 2^(LOG2(E)*X)
	jsr	FMULT
	lda	FACEXTENSION	; NON-STANDARD ROUNDING HERE
	adc	#$50		; ROUND UP IF EXTENSION > $AF
	bcc	@1		; NO, DON'T ROUND UP
	jsr	INCREMENT_MANTISSA
@1:	sta	ARGEXTENSION	; STRANGE VALUE
	jsr	MAF		; COPY FAC INTO ARG
	lda	FAC		; MAXIMUM EXPONENT IS < 128
	cmp	#$88		; WITHIN RANGE?
	bcc	@3		; YES
@2:	jsr	OUTOFRNG	; OVERFLOW IF +, RETURN 0.0 IF -
@3:	jsr	INT		; GET INT(FAC)
	lda	CHARAC		; THIS IS THE INETGRAL PART OF THE POWER
	clc			; ADD TO EXPONENT BIAS + 1
	adc	#$81
	beq	@2		; OVERFLOW
	sec			; BACK OFF TO NORMAL BIAS
	sbc	#1
	pha			; SAVE EXPONENT
; ----------------------------------------------------------------------------
	ldx	#5		; SWAP ARG AND FAC
@4:	lda	ARG,x
	ldy	FAC,x
	sta	FAC,x
	sty	ARG,x
	dex
	bpl	@4
	lda	ARGEXTENSION
	sta	FACEXTENSION
	jsr	FSUBT		; POWER-INT(POWER) --> FRACTIONAL PART
	jsr	NEGOP
	lda	#<POLY_EXP
	ldy	#>POLY_EXP
	jsr	POLYNOMIAL	; COMPUTE F(X) ON FRACTIONAL PART
	lda	#0
	sta	SGNCPR
	pla			; GET EXPONENT
	jmp	ADD_EXPONENTS1


; ----------------------------------------------------------------------------
; ODD POLYNOMIAL SUBROUTINE
;
; F(X) = X * P(X^2)
;
; WHERE:  X IS VALUE IN FAC
;	Y,A POINTS AT COEFFICIENT TABLE
;	FIRST BYTE OF COEFF. TABLE IS N
;	COEFFICIENTS FOLLOW, HIGHEST POWER FIRST
;
; P(X^2) COMPUTED USING NORMAL POLYNOMIAL SUBROUTINE
; ----------------------------------------------------------------------------
POLYNOMIAL_ODD:
	sta	SERPNT		; SAVE ADDRESS OF COEFFICIENT TABLE
	sty	SERPNT+1
	jsr	STORE_FAC_IN_TEMP1_ROUNDED
	lda	#TEMP1		; Y=0 ALREADY, SO Y,A POINTS AT TEMP1
	jsr	FMULT		; FORM X^2
	jsr	SERMAIN		; DO SERIES IN X^2
	lda	#<TEMP1		; GET X AGAIN
	ldy	#>TEMP1
	jmp	FMULT		; MULTIPLY X BY P(X^2) AND EXIT


; ----------------------------------------------------------------------------
; NORMAL POLYNOMIAL SUBROUTINE
;
; P(X) = C(0)*X^N + C(1)*X^(N-1) + ... + C(N)
;
; WHERE:  X IS VALUE IN FAC
;	Y,A POINTS AT COEFFICIENT TABLE
;	FIRST BYTE OF COEFF. TABLE IS N
;	COEFFICIENTS FOLLOW, HIGHEST POWER FIRST
; ----------------------------------------------------------------------------
POLYNOMIAL:
	sta	SERPNT		; POINTER TO COEFFICIENT TABLE
	sty	SERPNT+1
; ----------------------------------------------------------------------------
SERMAIN:
	jsr	STORE_FAC_IN_TEMP2_ROUNDED
	lda	(SERPNT),y	; GET N
	sta	SERLEN		; SAVE N
	ldy	SERPNT		; BUMP PNTR TO HIGHEST COEFFICIENT
	iny			; AND GET PNTR INTO Y,A
	tya
	bne	@1
	inc	SERPNT+1
@1:	sta	SERPNT
	ldy	SERPNT+1
@2:	jsr	FMULT		; ACCUMULATE SERIES TERMS
	lda	SERPNT		; BUMP PNTR TO NEXT COEFFICIENT
	ldy	SERPNT+1
	clc
	adc	#5
	bcc	@3
	iny
@3:	sta	SERPNT
	sty	SERPNT+1
	jsr	FADD		; ADD NEXT COEFFICIENT
	lda	#TEMP2		; POINT AT X AGAIN
	ldy	#0
	dec	SERLEN		; IF SERIES NOT FINISHED,
	bne	@2		; THEN ADD ANOTHER TERM
RTS19:  rts			; FINISHED

; ----------------------------------------------------------------------------
CONRND1:
	.byte	$98,$35,$44,$7A		; THESE ARE MISSING ONE BYTE FOR FP VALUES

CONRND2:
	.byte	$68,$28,$B1,$46


; ----------------------------------------------------------------------------
; "RND" FUNCTION
; ----------------------------------------------------------------------------
RND:	jsr	SIGN		; REDUCE ARGUMENT TO -1, 0, OR +1
	tax			; SAVE ARGUMENT
	bmi	@1		; = -1, USE CURRENT ARGUMENT FOR SEED
	lda	#<RNDSEED	; USE CURRENT SEED
	ldy	#>RNDSEED
	jsr	LOAD_FAC_FROM_YA
	txa			; RECALL SIGN OF ARGUMENT
	beq	RTS19		; =0, RETURN SEED UNCHANGED
	lda	#<CONRND1	; VERY POOR RND ALGORITHM
	ldy	#>CONRND1
	jsr	FMULT
	lda	#<CONRND2	; ALSO, CONSTANTS ARE TRUNCATED
	ldy	#>CONRND2
	jsr	FADD
@1:	ldx	FAC+4		; SHUFFLE HI AND LO BYTES
	lda	FAC+1		; TO SUPPOSEDLY MAKE IT MORE RANDOM
	sta	FAC+4
	stx	FAC+1
	lda	#0		; MAKE IT POSITIVE
	sta	FACSIGN
	lda	FAC		; A SOMEWHAT RANDOM EXTENSION
	sta	FACEXTENSION
	lda	#$80		; EXPONENT TO MAKE VALUE < 1.0
	sta	FAC
	jsr	NORMALIZE_FAC2
	ldx	#<RNDSEED	; MOVE FAC TO RND SEED
	ldy	#>RNDSEED
	jmp	STORE_FAC_AT_YX_ROUNDED


; ----------------------------------------------------------------------------
; GENERIC COPY OF CHRGET SUBROUTINE, WHICH
; IS COPIED INTO $00B1...$00C8 DURING INITIALIZATION
;
; CORNELIS BONGERS DESCRIBED SEVERAL IMPROVEMENTS 
; TO CHRGET IN MICRO MAGAZINE OR CALL A.P.P.L.E.
; (I DON'T REMEMBER WHICH OR EXACTLY WHEN)
; ----------------------------------------------------------------------------
GENERIC_CHRGET:
	inc	TXTPTR
	bne	@1
	inc	TXTPTR+1
@1:	lda	$EA60		; ACTUAL ADDRESS FILLED IN LATER
	cmp	#':'		; EOS, ALSO TOP OF NUMERIC RANGE
	bcs	@2		; NOT NUMBER, MIGHT BE EOS
	cmp	#' '		; IGNORE BLANKS
	beq	GENERIC_CHRGET
	sec			; TEST FOR NUMERIC RANGE IN WAY THAT
	sbc	#'0'		; CLEARS CARRY IF CHAR IS DIGIT
	sec			; AND LEAVES CHAR IN A-REG
	sbc	#-'0'
@2:	rts


; ----------------------------------------------------------------------------
; INITIAL VALUE FOR RANDOM NUMBER, ALSO COPIED
; IN ALONG WITH CHRGET, BUT ERRONEOUSLY:
; THE LAST BYTE IS NOT COPIED
; ----------------------------------------------------------------------------
	.byte	$80,$4F,$C7,$52,$58	; APPROX. = .811635157
GENERIC_END:


; ----------------------------------------------------------------------------
COLDSTART:
	ldx	#$FF		; SET DIRECT MODE FLAG
	stx	CURLIN+1
	ldx	#$FB		; SET STACK POINTER, LEAVING ROOM FOR
	txs			; LINE BUFFER DURING PARSING
	lda	#<COLDSTART	; SET RESTART TO COLD.START
	ldy	#>COLDSTART	; UNTIL COLDSTART IS COMPLETED
	sta	GOWARM+1
	sty	GOWARM+2
	sta	GOSTROUTZ+1	; ALSO SECOND USER VECTOR...
	sty	GOSTROUTZ+2	; ..WE SIMPLY MUST FINISH COLD.START!
	lda	#$4C		; "JMP" OPCODE FOR 4 VECTORS
	sta	GOWARM		; WARM START
	sta	GOSTROUTZ	; ANYONE EVER USE THIS ONE?
	sta	JMPADRS		; USED BY FUNCTIONS (JSR JMPADRS)

; ----------------------------------------------------------------------------
; MOVE GENERIC CHRGET AND RANDOM SEED INTO PLACE
;
; NOTE THAT LOOP VALUE IS WRONG! 
; THE LAST BYTE OF THE RANDOM SEED IS NOT
; COPIED INTO PAGE ZERO!
; ----------------------------------------------------------------------------
	ldx	#GENERIC_END-GENERIC_CHRGET-1
@1:	lda	GENERIC_CHRGET-1,x
	sta	CHRGET-1,x
	dex
	bne	@1
; ----------------------------------------------------------------------------
	txa			; A=0
	sta	SHIFTSIGNEXT
	sta	LASTPT+1
	pha			; PUT $00 ON STACK (WHAT FOR?)
	lda	#3		; SET LENGTH OF TEMP. STRING DESCRIPTORS
	sta	DSCLEN		; FOR GARBAGE COLLECTION SUBROUTINE
	jsr	CRDO		; PRINT <RETURN>
	lda	#1		; SET UP FAKE FORWARD LINK
	sta	INPUTBUFFER-3
	sta	INPUTBUFFER-4
	ldx	#TEMPST		; INIT INDEX TO TEMP STRING DESCRIPTORS
	stx	TEMPPT
; ----------------------------------------------------------------------------
; FIND HIGH END OF RAM
; ----------------------------------------------------------------------------
	lda	#<$0800		; SET UP POINTER TO LOW END OF RAM
	ldy	#>$0800
	sta	LINNUM
	sty	LINNUM+1
	ldy	#0
@2:	inc	LINNUM+1	; TEST FIRST BYTE OF EACH PAGE
	lda	(LINNUM),y	; BY COMPLEMENTING IT AND WATCHING
	eor	#$FF		; IT CHANGE THE SAME WAY
	sta	(LINNUM),y
	cmp	(LINNUM),y	; ROM OR EMPTY SOCKETS WON'T TRACK
	bne	@3		; NOT RAM HERE
	eor	#$FF		; RESTORE ORIGINAL VALUE
	sta	(LINNUM),y
	cmp	(LINNUM),y	; DID IT TRACK AGAIN?
	beq	@2		; YES, STILL IN RAM
@3:	ldy	LINNUM		; NO, END OF RAM
	lda	LINNUM+1
	and	#$F0		; FORCE A MULTIPLE OF 4096 BYTES
	sty	MEMSIZ		; (BAD RAM MAY HAVE YIELDED NON-MULTIPLE)
	sta	MEMSIZ+1
	sty	FRETOP		; SET HIMEM AND BOTTOM OF STRINGS
	sta	FRETOP+1
	ldx	#<$0800		; SET PROGRAM POINTER TO $0800
	ldy	#>$0800
	stx	TXTTAB
	sty	TXTTAB+1
	ldy	#0		; TURN OFF SEMI-SECRET LOCK FLAG
	sty	LOCK
	tya			; A=0 TOO
	sta	(TXTTAB),y	; FIRST BYTE IN PROGRAM SPACE = 0
	inc	TXTTAB		; ADVANCE PAST THE $00
	bne	@4
	inc	TXTTAB+1
@4:	lda	TXTTAB
	ldy	TXTTAB+1
	jsr	REASON		; SET REST OF POINTERS UP
	jsr	SCRTCH		; MORE POINTERS
	lda	#<STROUT	; PUT CORRECT ADDRESSES IN TWO
	ldy	#>STROUT	; USER VECTORS
	sta	GOSTROUTZ+1
	sty	GOSTROUTZ+2
	lda	#<RESTART
	ldy	#>RESTART
	sta	GOWARM+1
	sty	GOWARM+2
	jmp	(GOWARM+1)	; SILLY, WHY NOT JUST "JMP RESTART"


; ----------------------------------------------------------------------------
; "CALL" STATEMENT
;
; EFFECTIVELY PERFORMS A "JSR" TO THE SPECIFIED
; ADDRESS, WITH THE FOLLOWING REGISTER CONTENTS:
; 	(A,Y) = CALL ADDRESS
;	(X)   = $9D
;
; THE CALLED ROUTINE CAN RETURN WITH "RTS",
; AND APPLESOFT WILL CONTINUE WITH THE NEXT
; STATEMENT.
; ----------------------------------------------------------------------------
CALL:	jsr	FRMNUM		; EVALUATE EXPRESSION FOR CALL ADDRESS
	jsr	GETADR		; CONVERT EXPRESSION TO 16-BIT INTEGER
	jmp	(LINNUM)	; IN LINNUM, AND JUMP THERE.


; ----------------------------------------------------------------------------
; "HIMEM:" STATEMENT
; ----------------------------------------------------------------------------
HIMEM:	jsr	FRMNUM		; GET VALUE SPECIFIED FOR HIMEM
	jsr	GETADR		; AS 16-BIT INTEGER
	lda	LINNUM		; MUST BE ABOVE VARIABLES AND ARRAYS
	cmp	STREND
	lda	LINNUM+1
	sbc	STREND+1
	bcs	SETHI		; IT IS ABOVE THEM
JMM:	jmp	MEMERR		; NOT ENOUGH MEMORY
SETHI:	lda	LINNUM		; STORE NEW HIMEM: VALUE
	sta	MEMSIZ
	sta	FRETOP		; NOTE THAT "HIMEM:" DOES NOT
	lda	LINNUM+1	; CLEAR STRING VARIABLES.
	sta	MEMSIZ+1	; THIS COULD BE DISASTROUS.
	sta	FRETOP+1
	rts


; ----------------------------------------------------------------------------
; "LOMEM:" STATEMENT
; ----------------------------------------------------------------------------
LOMEM:	jsr	FRMNUM		; GET VALUE SPECIFIED FOR LOMEM
	jsr	GETADR		; AS 16-BIT INTEGER IN LINNUM
	lda	LINNUM		; MUST BE BELOW HIMEM
	cmp	MEMSIZ
	lda	LINNUM+1
	sbc	MEMSIZ+1
	bcs	JMM		; ABOVE HIMEM, MEMORY ERROR
	lda	LINNUM		; MUST BE ABOVE PROGRAM
	cmp	VARTAB
	lda	LINNUM+1
	sbc	VARTAB+1
	bcc	JMM		; NOT ABOVE PROGRAM, ERROR
	lda	LINNUM		; STORE NEW LOMEM VALUE
	sta	VARTAB
	lda	LINNUM+1
	sta	VARTAB+1
	jmp	CLEARC		; LOMEM CLEARS VARIABLES AND ARRAYS


; ----------------------------------------------------------------------------
; "ON ERR GO TO" STATEMENT
; ----------------------------------------------------------------------------
ONERR:	lda	#TOKEN_GOTO	; MUST BE "GOTO" NEXT
	jsr	SYNCHR
	lda	TXTPTR		; SAVE TXTPTR FOR HANDLERR
	sta	TXTPSV
	lda	TXTPTR+1
	sta	TXTPSV+1
	sec			; SET SIGN BIT OF ERRFLG
	ror	ERRFLG
	lda	CURLIN		; SAVE LINE # OF CURRENT LINE
	sta	CURLSV
	lda	CURLIN+1
	sta	CURLSV+1
	jsr	REMN		; IGNORE REST OF LINE <<<WHY?>>>
	jmp	ADDON		; CONTINUE PROGRAM


; ----------------------------------------------------------------------------
; ROUTINE TO HANDLE ERRORS IF ONERR GOTO ACTIVE
; ----------------------------------------------------------------------------
HANDLERR:
	stx	ERRNUM		; SAVE ERROR CODE NUMBER
	ldx	REMSTK		; GET STACK PNTR SAVED AT NEWSTT
	stx	ERRSTK		; REMEMBER IT
	lda	CURLIN		; GET LINE # OF OFFENDING STATEMENT
	sta	ERRLIN		; SO USER CAN SEE IT IF DESIRED
	lda	CURLIN+1
	sta	ERRLIN+1
	lda	OLDTEXT		; ALSO THE POSITION IN THE LINE
	sta	ERRPOS		; IN CASE USER WANTS TO "RESUME"
	lda	OLDTEXT+1
	sta	ERRPOS+1
	lda	TXTPSV		; SET UP TXTPTR TO READ TARGET LINE #
	sta	TXTPTR		; IN "ON ERR GO TO XXXX"
	lda	TXTPSV+1
	sta	TXTPTR+1
	lda	CURLSV
	sta	CURLIN		; LINE # OF "ON ERR" STATEMENT
	lda	CURLSV+1
	sta	CURLIN+1
	jsr	CHRGOT		; START CONVERSION
	jsr	GOTO		; GOTO SPECIFIED ONERR LINE
	jmp	NEWSTT


; ----------------------------------------------------------------------------
; "RESUME" STATEMENT
; ----------------------------------------------------------------------------
RESUME:	lda	ERRLIN		; RESTORE LINE # AND TXTPTR
	sta	CURLIN		; TO RE-TRY OFFENDING LINE
	lda	ERRLIN+1
	sta	CURLIN+1
	lda	ERRPOS
	sta	TXTPTR
	lda	ERRPOS+1
	sta	TXTPTR+1
	ldx	ERRSTK		; RETRIEVE STACK PNTR AS IT WAS
	txs			; BEFORE STATEMENT SCANNED
	jmp	NEWSTT		; DO STATEMENT AGAIN

