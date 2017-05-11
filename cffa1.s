; CFFA1 I/O routines for Applesoft Lite
; by Tom Greene 
; 8-May-2008

.setcpu "6502"
.segment "BASIC"

.include "zeropage.s"

.importzp ERR_SYNTAX, ERR_NOCFFA
.import ERROR, FIX_LINKS
.export CFFALoad, CFFASave, CFFAMenu

; ----------------------------------------------------------------------------
; CFFA1 firmware addresses

;CFFA_ID1	:= $AFFC	; CFFA1 API documentation says AFFC and AFFD...
;CFFA_ID2	:= $AFFD
CFFA_ID1	:= $AFDC	; But the CFFA1 firmware says AFDC and AFDD...
CFFA_ID2	:= $AFDD
CFFA_MENU	:= $9006	; Entry point for the CFFA1 menu
CFFA_API	:= $900C	; Entry point for the CFFA1 API

; ----------------------------------------------------------------------------
; CFFA1 API functions

CFFA1_DisplayError	= $04	; Displays error message for error value in A
CFFA1_WriteFile		= $20
CFFA1_ReadFile		= $22

; ----------------------------------------------------------------------------
; CFFA1 API parameters in the ZP

Destination	:= $00			; File load/save address
Filename	:= $02			; Pointer to file name
Filetype	:= $06			; ProDOS file type
AuxType		:= $07			; ProDOS file auxtype
FileSize	:= $09			; File size 

; ----------------------------------------------------------------------------
; Scratch memory used during API calls

ZPTemp		:= $0380		; ZP locations get backed up here
CFFAFileName	:= $03C0		; File name string

; ----------------------------------------------------------------------------
; ProDOS file type value for write operations
; The CFFA1 doesn't really care, so this is just for show.

SaveType	= $F8			; F8 = "PRG"?

; ----------------------------------------------------------------------------


; ----------------------------------------------------------------------------
; See if CFFA1 card is present and display error if not
; ----------------------------------------------------------------------------
CheckCFFA:
	ldy	CFFA_ID1
	cpy	#$CF		
	bne	CFFAErr
	ldy	CFFA_ID2
	cpy	#$FA
	bne	CFFAErr
	rts


; ----------------------------------------------------------------------------
; Bring up the CFFA1 menu 
; ----------------------------------------------------------------------------
CFFAMenu:
	jsr	CheckCFFA
	jmp	CFFA_MENU


; ----------------------------------------------------------------------------
; This sets up the zero page locations file name for the read/write calls
; ----------------------------------------------------------------------------
APISetup:
	jsr	CheckCFFA	; Make sure CFFA card is present first
	ldy	#0
@1:	lda	GOWARM,y	; Back up first 12 bytes of the ZP
	sta	ZPTemp,y	; so they can used during CFFA API call
	iny
	cpy	#12
	bne	@1
GetFileName:			; Get file name from input line
	dec	TXTPTR
	ldy	#0
@1:	jsr	CHRGET		; Get next character from the input line
	beq	@2		; Is it null (EOL)?
	sta	CFFAFileName+1,y	; Not EOL, store it in filename string
	iny
	cpy	#15			; 15 chars yet?
	bne	@1			; no, go back for more
@2:	cpy	#0			; Read 15 chars or EOL, did we get anything?
	beq	SynErr			; No, syntax error
	sty	CFFAFileName		; Store file name length
	lda	#<CFFAFileName		; Load address of file name string
	ldy	#>CFFAFileName
	sta	Filename		; and store it for API call
	sty	Filename+1
	lda	PRGEND		; Set up file size
	sbc	TXTTAB		; (PRGEND - TXTTAB)
	sta	FileSize
	lda	PRGEND+1
	sbc	TXTTAB+1
	sta	FileSize+1
	lda	TXTTAB		; Set up start address and auxtype
	ldy	TXTTAB+1	; (these will be the same)
	sta	Destination
	sta	AuxType
	sty	Destination+1
	sty	AuxType+1
	lda	#SaveType	; Set up ProDOS file type
	sta	Filetype
	rts


; ----------------------------------------------------------------------------
; Display an error message if something went wrong during APISetup
; Uses the Applesoft ERROR routine
; ----------------------------------------------------------------------------
CFFAErr:
	ldx	#ERR_NOCFFA
	.byte	$2C		; Bogus BIT instruction
SynErr:
	ldx	#ERR_SYNTAX
	jmp	ERROR		; Jump to Applesoft ERROR routine


; ----------------------------------------------------------------------------
; Restores the first 12 bytes of the ZP which were saved during APISetup
; ----------------------------------------------------------------------------
RestoreZP:
	ldy	#0
@1:	lda	ZPTemp,y	; Load byte from temporary storage
	sta	GOWARM,y	; put it back in its original location
	iny
	cpy	#12		; Repeat for next 11 bytes	
	bne	@1
	rts


; ----------------------------------------------------------------------------
; Write a file to the CFFA card (SAVE command)
; ----------------------------------------------------------------------------
CFFASave:
	jsr	APISetup	; Set up zero page locations for API call
	ldx	#CFFA1_WriteFile; Select WriteFile API function
	jsr	DoCFFACall	; Do it
@1:	jsr	RestoreZP	; put ZP back together
@2:	rts


; ----------------------------------------------------------------------------
; Read file from CFFA, then fix up Applesoft zero page to point to the 
; loaded program (LOAD command)
; ----------------------------------------------------------------------------
CFFALoad:
	jsr	APISetup
	ldx	#CFFA1_ReadFile	; Select ReadFile API function
	jsr	DoCFFACall	; Do it
	lda	TXTTAB		; Compute program end address
	adc	FileSize	; (Add file size to program start)
	sta	VARTAB		; Store end address
	lda	TXTTAB+1	
	adc	FileSize+1
	sta	VARTAB+1
	jsr	RestoreZP	; Put the zero page back
	jmp	FIX_LINKS	; Done loading, fix pointers and restart


; ----------------------------------------------------------------------------
; Call the CFFA1 API and display error message if one occurred
; CFFA function number is passed in X
; CFFA API returns error status in C, error number in A
; ----------------------------------------------------------------------------
DoCFFACall:			
	jsr	CFFA_API		; Call CFFA API
	ldx	#CFFA1_DisplayError	; Set next command to show error message
	bcs	DoCFFACall		; Call API again if error occurred
	rts


