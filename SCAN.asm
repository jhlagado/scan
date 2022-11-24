; *************************************************************************
;
;       Siena Programming Language for the Z80 
;
;       by John Hardy 2022
;
;       Incorporating code from the MINT project by Ken Boak and Craig Jones. 
;
;       GNU GENERAL PUBLIC LICENSE                   Version 3, 29 June 2007
;
;       see the LICENSE file in this repo for more information 
;
; *****************************************************************************

        DSIZE       EQU $80
        RSIZE       EQU $80
        LSIZE       EQU $80
        TIBSIZE     EQU $100	; 256 bytes , along line!
        TRUE        EQU 1		; not FF, for Siena
        FALSE       EQU 0
        EMPTY       EQU 0		; for an empty macro, ctrl-<something>=macro, ie ctrl-h = backspace macros (in Siena)

        mintDataSize      EQU 26*2*2	; A..Z, a..z words

.macro LITDAT,len
        DB len
.endm

.macro REPDAT,len,data			; compress the command tables
        
        DB (len | $80)
        DB data
.endm

.macro ENDDAT
        DB 0
.endm

; **************************************************************************
; Page 0  Initialisation
; **************************************************************************		

		.ORG ROMSTART + $180		; 0+180 put Siena code from here	

; **************************************************************************
; this code must not span pages
; **************************************************************************
macros:

; ***********************************************************************
; Initial values for user mintVars		
; ***********************************************************************		
iAltVars:			            ; value copied into tables
        DW dStack               ; a vS0 start of datastack			
        DW FALSE                ; b vBase16 
        DW 0                    ; c vTIBPtr an offset to the tib
        DW 0                    ; d 
        DW 65                   ; e vLastDef "A" last command u defined
        DW 0                    ; f 
        DW page6                ; g 256 bytes limits
        DW HEAP                 ; h vHeapPtr \h start of the free mem



iOpcodes:
        LITDAT 4		; macros for compression
        DB lsb(exit_)    ;   NUL get least signif byte of address exit_
        DB lsb(nop_)     ;   SOH 
        DB lsb(nop_)     ;   STX 
        DB lsb(etx_)     ;   ETX 

        LITDAT 29
        ; REPDAT 29, lsb(nop_)

        DB lsb(nop_)    ;               
        DB lsb(nop_)    ;               
        DB lsb(nop_)    ;               
        DB lsb(nop_)    ;               
        DB lsb(nop_)    ;               
        DB lsb(nop_)    ;               
        DB lsb(nop_)    ;               
        DB lsb(nop_)    ;               
        DB lsb(nop_)    ;               
        DB lsb(nop_)    ;               
        DB lsb(nop_)    ;               
        DB lsb(nop_)    ;               
        DB lsb(nop_)    ;               
        DB lsb(nop_)    ;               
        DB lsb(nop_)    ;               
        DB lsb(nop_)    ;               
        DB lsb(nop_)    ;               
        DB lsb(nop_)    ;               
        DB lsb(nop_)    ;               
        DB lsb(nop_)    ;               
        DB lsb(nop_)    ;               
        DB lsb(nop_)    ;               
        DB lsb(nop_)    ;               
        DB lsb(nop_)    ;               
        DB lsb(nop_)    ;               
        DB lsb(nop_)    ;               
        DB lsb(nop_)    ;               
        DB lsb(nop_)    ;               
        DB lsb(nop_)    ;               

        LITDAT 15
        DB lsb(nop_)    ;   !            
        DB lsb(nop_)    ;   "
        DB lsb(nop_)    ;   #
        DB lsb(hex_)    ;   $            
        DB lsb(nop_)    ;   %            
        DB lsb(and_)    ;   &
        DB lsb(nop_)    ;   '
        DB lsb(nop_)    ;   (        
        DB lsb(nop_)    ;   )
        DB lsb(mul_)    ;   *            
        DB lsb(add_)    ;   +
        DB lsb(nop_)    ;   ,            
        DB lsb(sub_)    ;   -
        DB lsb(dot_)    ;   .
        DB lsb(nop_)    ;   /	

        ; REPDAT 10, lsb(num_)		; 10 x repeat lsb of add to the num routine 
        LITDAT 10
        DB lsb(num_)    ;   0               
        DB lsb(num_)    ;   1             
        DB lsb(num_)    ;   2             
        DB lsb(num_)    ;   3             
        DB lsb(num_)    ;   4             
        DB lsb(num_)    ;   5             
        DB lsb(num_)    ;   6             
        DB lsb(num_)    ;   7             
        DB lsb(num_)    ;   8             
        DB lsb(num_)    ;   9             

        LITDAT 7
        DB lsb(nop_)    ;   :        
        DB lsb(nop_)    ;   ;
        DB lsb(lt_)     ;   <
        DB lsb(eq_)     ;   =            
        DB lsb(gt_)     ;   >            
        DB lsb(nop_)    ;   ?   ( -- val )  read a char from input
        DB lsb(nop_)    ;   @       

        ; REPDAT 26, lsb(call_)		; call a command A, B ....Z
        LITDAT 26
        DB lsb(call_)   ;   A               
        DB lsb(call_)   ;   B               
        DB lsb(call_)   ;   C               
        DB lsb(call_)   ;   D               
        DB lsb(call_)   ;   E               
        DB lsb(call_)   ;   F               
        DB lsb(call_)   ;   G               
        DB lsb(call_)   ;   H               
        DB lsb(call_)   ;   I               
        DB lsb(call_)   ;   J               
        DB lsb(call_)   ;   K               
        DB lsb(call_)   ;   L               
        DB lsb(call_)   ;   M               
        DB lsb(call_)   ;   N               
        DB lsb(call_)   ;   O               
        DB lsb(call_)   ;   P               
        DB lsb(call_)   ;   Q               
        DB lsb(call_)   ;   R               
        DB lsb(call_)   ;   S               
        DB lsb(call_)   ;   T               
        DB lsb(call_)   ;   U               
        DB lsb(call_)   ;   V               
        DB lsb(call_)   ;   W               
        DB lsb(call_)   ;   X               
        DB lsb(call_)   ;   Y               
        DB lsb(call_)   ;   Z              

        LITDAT 6
        DB lsb(nop_)    ;   [
        DB lsb(nop_)    ;   \
        DB lsb(nop_)    ;   ]
        DB lsb(nop_)    ;   ^
        DB lsb(nop_)    ;   _
        DB lsb(nop_)    ;   `    	        

        ; REPDAT 26, lsb(var_)		
        LITDAT 26
        DB lsb(a_)      ;   a               
        DB lsb(var_)    ;   b            
        DB lsb(c_)      ;   c            
        DB lsb(d_)      ;   d            
        DB lsb(e_)      ;   e            
        DB lsb(f_)      ;   f            
        DB lsb(g_)      ;   g            
        DB lsb(var_)    ;   h            
        DB lsb(i_)      ;   i            
        DB lsb(var_)    ;   j            
        DB lsb(k_)      ;   k            
        DB lsb(l_)      ;   l            
        DB lsb(m_)      ;   m            
        DB lsb(m_)      ;   n            
        DB lsb(o_)      ;   o            
        DB lsb(p_)      ;   p            
        DB lsb(var_)    ;   q            
        DB lsb(r_)      ;   r            
        DB lsb(s_)      ;   s            
        DB lsb(var_)    ;   t            
        DB lsb(u_)      ;   u            
        DB lsb(var_)    ;   v            
        DB lsb(w_)      ;   w            
        DB lsb(x_)      ;   x            
        DB lsb(var_)    ;   y            
        DB lsb(var_)    ;   z            

        LITDAT 5
        DB lsb(nop_)    ;   {
        DB lsb(or_)     ;   |            
        DB lsb(nop_)    ;   }            
        DB lsb(nop_)    ;   ~             
        DB lsb(nop_)    ;   DEL	

        LITDAT 32
        DB lsb(EMPTY)       ; NUL ^@        
        DB lsb(EMPTY)       ; SOH ^A  1
        DB lsb(EMPTY)       ; STX ^B  2
        DB lsb(EMPTY)       ; ETX ^C  3
        DB lsb(EMPTY)       ; EOT ^D  4
        DB lsb(EMPTY)       ; ENQ ^E  5
        DB lsb(EMPTY)       ; ACK ^F  6
        DB lsb(EMPTY)       ; BEL ^G  7 
        DB lsb(EMPTY)       ; BS  ^H  8
        DB lsb(EMPTY)       ; TAB ^I  9
        DB lsb(EMPTY)       ; LF  ^J 10
        DB lsb(EMPTY)       ; VT  ^K 11
        DB lsb(EMPTY)       ; FF  ^L 12
        DB lsb(EMPTY)       ; CR  ^M 13
        DB lsb(EMPTY)       ; SO  ^N 14
        DB lsb(EMPTY)       ; SI  ^O 15
        DB lsb(EMPTY)       ; DLE ^P 16
        DB lsb(EMPTY)       ; ^Q               
        DB lsb(EMPTY)       ; ^R               
        DB lsb(EMPTY)       ; ^S             
        DB lsb(EMPTY)       ; ^T             
        DB lsb(EMPTY)       ; ^U                
        DB lsb(EMPTY)       ; ^V            
        DB lsb(EMPTY)       ; ^W             
        DB lsb(EMPTY)       ; ^X              
        DB lsb(EMPTY)       ; ^Y             
        DB lsb(EMPTY)       ; ^Z             
        DB lsb(EMPTY)       ; ^[            
        DB lsb(EMPTY)       ; ^\            
        DB lsb(EMPTY)       ; ^]            
        DB lsb(EMPTY)       ; ^^            
        DB lsb(EMPTY)       ; ^_            

        LITDAT 5
        DB lsb(aNop_)       ;a0    SP  				;space
        DB lsb(aNop_)       ;a1    \!       			; this is a bug shud be lsb(cstore_)     
        DB lsb(aNop_)       ;a2    \"  				
        DB lsb(aNop_)       ;a3    \#  utility command		; table of special routines ie #5 etc				
        DB lsb(aNop_)       ;a4    \$  prints a newline to output	

        ; REPDAT 3, lsb(aNop_)
        LITDAT 3
        DB lsb(aNop_)       ;               
        DB lsb(aNop_)       ;               
        DB lsb(aNop_)       ;               

        LITDAT 8
        DB lsb(aNop_)       ;a8    \(  ( b -- )              
        DB lsb(aNop_)       ;a9    \)                
        DB lsb(aNop_)       ;aa    \*                
        DB lsb(aNop_)       ;ab    \+                
        DB lsb(aNop_)       ;ac    \,  ( b -- ) prints a char              
        DB lsb(aNop_)       ;ad    \-                
        DB lsb(aNop_)       ;ae    \.  ( b -- ) prints a string from add term by null char             
        DB lsb(aNop_)       ;af    \/                

        ; REPDAT 10, lsb(aNop_)
        LITDAT 10
        DB lsb(aNop_)    ;               
        DB lsb(aNop_)    ;               
        DB lsb(aNop_)    ;               
        DB lsb(aNop_)    ;               
        DB lsb(aNop_)    ;               
        DB lsb(aNop_)    ;               
        DB lsb(aNop_)    ;               
        DB lsb(aNop_)    ;               
        DB lsb(aNop_)    ;               
        DB lsb(aNop_)    ;               

        LITDAT 7
        DB lsb(aNop_)    ;ba    \:	return add of a anon def, \: 1 2 3;    \\ ret add of this                
        DB lsb(aNop_)       ;bb    \;                
        DB lsb(aNop_)       ;bc    \<  ( port -- val )
        DB lsb(aNop_)       ;bd    \=    
        DB lsb(aNop_)       ;be    \>  ( val port -- )
        DB lsb(aNop_)       ;bf    \?
        DB lsb(aNop_)     ;c0    \@      byte fetch

        ; REPDAT 26, lsb(aNop_)
        LITDAT 26
        DB lsb(aNop_)    ;               
        DB lsb(aNop_)    ;               
        DB lsb(aNop_)    ;               
        DB lsb(aNop_)    ;               
        DB lsb(aNop_)    ;               
        DB lsb(aNop_)    ;               
        DB lsb(aNop_)    ;               
        DB lsb(aNop_)    ;               
        DB lsb(aNop_)    ;               
        DB lsb(aNop_)    ;               
        DB lsb(aNop_)    ;               
        DB lsb(aNop_)    ;               
        DB lsb(aNop_)    ;               
        DB lsb(aNop_)    ;               
        DB lsb(aNop_)    ;               
        DB lsb(aNop_)    ;               
        DB lsb(aNop_)    ;               
        DB lsb(aNop_)    ;               
        DB lsb(aNop_)    ;               
        DB lsb(aNop_)    ;               
        DB lsb(aNop_)    ;               
        DB lsb(aNop_)    ;               
        DB lsb(aNop_)    ;               
        DB lsb(aNop_)    ;               
        DB lsb(aNop_)    ;               
        DB lsb(aNop_)    ;               

        LITDAT 6
        DB lsb(aNop_)   ;     \[
        DB lsb(aNop_)   ;     \\  comment text, skips reading until end of line
        DB lsb(aNop_)   ;     \]
        DB lsb(aNop_)   ;     \^  ( -- ? ) execute Siena definition a is address of Siena code
        DB lsb(aNop_)   ;       \_  ( b -- ) conditional early return - stop everything           
        DB lsb(aNop_)   ;     \`  ( -- adr ) defines a string \` string ` then use \. to prt            

        ; REPDAT 8, lsb(altVar_)  ;e1	\a...\h
        LITDAT 8
        DB lsb(aNop_)
        DB lsb(aNop_)
        DB lsb(aNop_)
        DB lsb(aNop_)
        DB lsb(aNop_)
        DB lsb(aNop_)
        DB lsb(aNop_)
        DB lsb(aNop_)
    
        LITDAT 2
        DB lsb(aNop_)   ; returns index variable of current loop          
        DB lsb(aNop_)   ; returns index variable of outer loop     \i+6     

        ; REPDAT 16, lsb(altVar_)		; \k...\z
        LITDAT 16
        DB lsb(aNop_)
        DB lsb(aNop_)
        DB lsb(aNop_)
        DB lsb(aNop_)
        DB lsb(aNop_)
        DB lsb(aNop_)
        DB lsb(aNop_)
        DB lsb(aNop_)
        DB lsb(aNop_)
        DB lsb(aNop_)
        DB lsb(aNop_)
        DB lsb(aNop_)
        DB lsb(aNop_)
        DB lsb(aNop_)
        DB lsb(aNop_)
        DB lsb(aNop_)

        LITDAT 5
        DB lsb(aNop_)        ;       { ( -- n ) pop from Siena return stack 
        DB lsb(aNop_)        ;                  
        DB lsb(aNop_)       ;       } ( n -- ) push to return stack           
        DB lsb(aNop_)       ;       ~ ( b -- ) conditional break from loop            
        DB lsb(aNop_)        ;       DEL

        ENDDAT 

etx:                                ;=12
        ld HL,-DSTACK
        add HL,SP
        jr NC,etx1
        ld SP,DSTACK
etx1:
        jr interpret

start:
        ld SP,DSTACK		    ; start of Siena
        call init		        ; setups
        call printStr		    ; prog count to stack, put code line 235 on stack then call print
        .cstr "Siena V0.0\r\n"

interpret:
        call prompt

        ld BC,0                 ; load BC with offset into TIB, decide char into tib or execute or control         
        ld (vTIBPtr),BC

interpret2:                     ; calc nesting (a macro might have changed it)
        ld E,0                  ; initilize nesting value
        push BC                 ; save offset into TIB, 
                                ; BC is also the count of chars in TIB
        ld HL,TIB               ; HL is start of TIB
        jr interpret4

interpret3:
        ld A,(HL)               ; A = char in TIB
        inc HL                  ; inc pointer into TIB
        dec BC                  ; dec count of chars in TIB
        call nesting            ; update nesting value

interpret4:
        ld A,C                  ; is count zero?
        or B
        jr NZ, interpret3       ; if not loop
        pop BC                  ; restore offset into TIB
; *******************************************************************         
; Wait for a character from the serial input (keyboard) 
; and store it in the text buffer. Keep accepting characters,
; increasing the instruction pointer BC - until a newline received.
; *******************************************************************

waitchar:   
        call getchar            ; loop around waiting for character from serial port
        cp $20			; compare to space
        jr NC,waitchar1		; if >= space, if below 20 set cary flag
        cp $0                   ; is it end of string? null end of string
        jr Z,waitchar4
        cp '\r'                 ; carriage return? ascii 13
        jr Z,waitchar3		; if anything else its macro/control 
        ; ld D,0
macro:                          ;=25
        ld (vTIBPtr),BC
        ld HL,ctrlCodes
        add A,L			;look up key of macros
        ld L,A
        ld E,(HL)
        ld A,E
        or A
        jr Z,macro1
        ld D,msb(macros)
        push DE
        call ENTER		;Siena go operation and jump to it
        .cstr "\\^"
macro1:
        ld BC,(vTIBPtr)
        jr interpret2

waitchar1:
        ld HL,TIB
        add HL,BC
        ld (HL),A               ; store the character in textbuf
        inc BC
        call putchar            ; echo character to screen
        call nesting
        jr  waitchar            ; wait for next character

waitchar3:
        ld HL,TIB
        add HL,BC
        ld (HL),"\r"            ; store the crlf in textbuf
        inc HL
        ld (HL),"\n"            
        inc HL                  ; ????
        inc BC
        inc BC
        call crlf               ; echo character to screen
        ld A,E                  ; if zero nesting append and ETX after \r
        or A
        jr NZ,waitchar
        ld (HL),$03             ; store end of text ETX in text buffer 
        inc BC

waitchar4:    
        ld (vTIBPtr),BC
        ld BC,TIB               ; Instructions stored on heap at address HERE, we pressed enter
        dec BC

; ********************************************************************************
;
; Dispatch Routine.
;
; Get the next character and form a 1 byte jump address
;
; This target jump address is loaded into HL, and using jp (HL) to quickly 
; jump to the selected function.
;
; Individual handler routines will deal with each category:
;
; 1. Detect characters A-Z and jump to the User Command handler routine
;
; 2. Detect characters a-z and jump to the variable handler routine
;
; 3. All other characters are punctuation and cause a jump to the associated
; primitive code.
;
; Instruction Pointer IP BC is incremented
;
; *********************************************************************************

NEXT:                               ;=9 
        inc BC                      ;       Increment the IP
        ld A, (BC)                  ;       Get the next character and dispatch
        ld L,A                      ;       Index into table
        ld H,msb(opcodes)           ;       Start address of jump table         
        ld L,(HL)                   ;       get low jump address
        ld H,msb(page4)             ;       Load H with the 1st page address
        jp (HL)                     ;       Jump to routine

init:                           ;=68
        ld HL,LSTACK
        ld (vLoopSP),HL         ; Loop stack pointer stored in memory
        ld IX,RSTACK
        ld IY,NEXT		; IY provides a faster jump to NEXT
        ld HL,ialtVars
        ld DE,altVars
        ld BC,8 * 2
        ldir
        
        ld HL,mintData          ; init namespaces to 0 using ldir
        ld DE,HL
        inc DE
        ld (HL),0
        ld BC,mintDataSize
        ldir

initOps:
        ld HL, iOpcodes
        ld DE, opcodes
        ld BC, 256

initOps1:
        ld A,(HL)
        inc HL
        sla A                     
        ret Z
        jr C, initOps2
        srl A
        ld C,A
        ld B,0
        ldir
        jr initOps1
        
initOps2:        
        srl A
        ld B,A
        ld A,(HL)
        inc HL
initOps2a:
        ld (DE),A
        inc DE
        djnz initOps2a
        jr initOps1

enter:                              ;=9
        ld HL,BC
        call rpush                  ; save Instruction Pointer
        pop BC
        dec BC
        jp (IY)                    

printStr:                       ;=14
        ex (SP),HL		; swap			
        call putStr		
        inc HL			; inc past null
        ex (SP),HL		; put it back	
        ret

lookupRef:
        ld D,0
lookupRef0:
        cp "a"
        jr NC,lookupRef2
lookupRef1:
        sub "A"
        ld E,0
        jr lookupRef3        
lookupRef2:
        sub "a"
        ld E,26*2
lookupRef3:
        add A,A
        add A,E
        ld HL,mintData
        add A,L
        ld L,A
        ld A,0
        adc A,H
        ld H,A
        xor A
        or E                        ; sets Z flag if A-Z
        ret

absHL:
        bit 7,h
        ret z
        xor a  
        sub l  
        ld l,a
        sbc a,a  
        sub h  
        ld h,a
        ret

printdec:                           ;=34 ; removes leading zeros
        push BC
        ld BC,0
        ld DE,-10000
        call printdec1
        ld DE,-1000
        call printdec1
        ld DE,-100
        call printdec1
        ld E,-10
        call printdec1
        ld E,-1
        call printdec1
        pop BC
        ret
printdec1:	                        ;=24 
        ld B,'0'-1
printdec2:	    
        inc B
        add HL,DE
        jr C,printdec2
        sbc HL,DE
        ld A,'0'
        cp B
        jr NZ,printdec3
        xor A
        or C
        ret Z
        jr printdec4
printdec3:	    
        inc C
printdec4:	    
        ld A,B
        jp putchar

printhex:                           ;=31  
                                    ; Display HL as a 16-bit number in hex.
        push BC                     ; preserve the IP
        ld A,H
        call printhex2
        ld A,L
        call printhex2
        pop BC
        ret
printhex2:		                    
        ld	C,A
		rra 
		rra 
		rra 
		rra 
	    call printhex3
	    ld A,C
printhex3:		
        and	0x0F
		add	A,0x90
		daa
		adc	A,0x40
		daa
		jp putchar

; **************************************************************************             
; calculate nesting value
; A is char to be tested, 
; E is the nesting value (initially 0)
; E is increased by ( and [ 
; E is decreased by ) and ]
; E has its bit 7 toggled by `
; limited to 127 levels
; **************************************************************************             

nesting:                        ;=44
        cp '`'
        jr NZ,nesting1
        bit 7,E
        jr Z,nesting1a
        res 7,E
        ret
nesting1a: 
        set 7,E
        ret
nesting1:
        bit 7,E             
        ret NZ             
        cp ':'
        jr Z,nesting2
        cp '['
        jr Z,nesting2
        cp '('
        jr NZ,nesting3
nesting2:
        inc E
        ret
nesting3:
        cp ';'
        jr Z,nesting4
        cp ']'
        jr Z,nesting4
        cp ')'
        ret NZ
nesting4:
        dec E
        ret 
        
; **********************************************************************			 
; Page 4 primitive routines 
; **********************************************************************
        .align $100
page4:

and_:        
        pop     DE          ;     Bitwise and the top 2 elements of the stack
        pop     HL          ;    
        ld      A,E         ;   
        and     L           ;   
        ld      L,A         ;   
        ld      A,D         ;   
        and     H           ;   
and1:
        ld      H,A         ;   
        push    HL          ;    
        jp (IY)        ;   
        
                            ; 
or_: 		 
        pop     DE             ; Bitwise or the top 2 elements of the stack
        pop     HL
        ld      A,E
        or      L
        ld      L,A
        ld      A,D
        or      H
        jr and1

xor_:		 
        pop     DE              ; Bitwise xor the top 2 elements of the stack
xor1:
        pop     HL
        ld      A,E
        xor     L
        ld      L,A
        ld      A,D
        xor     H
        jr and1

inv_:				; Bitwise INVert the top member of the stack
        ld DE, $FFFF            ; by xoring with $FFFF
        jr xor1        
   
add_:                           ; add the top 2 members of the stack
        pop     DE                 
        pop     HL                 
        add     HL,DE              
        push    HL                 
        jp (IY)              
                                 
call_:
        ld A,(BC)
        call lookupRef1
        ld E,(HL)
        inc HL
        ld D,(HL)
        jp go1


dot_:       
        pop HL
        call printdec
dot2:
        ld A,' '           
        call putChar
        jp (IY)

hdot_:                          ; print hexadecimal
        pop     HL
        call printhex
        jr   dot2

drop_:                          ; Discard the top member of the stack
        pop     HL
        jp (IY)

dup_:        
        pop     HL              ; Duplicate the top member of the stack
        push    HL
        push    HL
        jp (IY)
etx_:
        jp ETX
        
exit_:
        inc BC			; store offests into a table of bytes, smaller
        ld DE,BC                
        call rpop               ; Restore Instruction pointer
        ld BC,HL
        ex DE,HL
        jp (HL)
        
fetch_:                         ; Fetch the value from the address placed on the top of the stack      
        pop HL              
fetch1:
        ld E,(HL)         
        inc HL             
        ld D,(HL)         
        push DE              
        jp (IY)           

hex_:   jp hex

key_:
        call getchar
        ld H,0
        ld L,A
        push HL
        jp (IY)

mul_:   jp mul      

nop_:       
        jp NEXT             ; hardwire white space to always go to NEXT (important for arrays)


over_:  
        pop HL              ; Duplicate 2nd element of the stack
        pop DE
        push DE
        push HL
        push DE              ; and push it to top of stack
        jp (IY)        
    
ret_:
        call rpop               ; Restore Instruction pointer
        ld BC,HL                
        jp (IY)             

rot_:                               ; a b c -- b c a
        pop DE                      ; a b                   de = c
        pop HL                      ; a                     hl = b
        ex (SP),HL                  ; b                     hl = a
        push DE                     ; b c             
        push HL                     ; b c a                         
        jp (IY)

;  Left shift { is multiply by 2		
shl_:   
        pop HL                  ; Duplicate the top member of the stack
        add HL,HL
        push HL                 ; shift left fallthrough into add_     
        jp (IY)                 ;   
    
				;  Right shift } is a divide by 2		
shr_:    
        pop HL                  ; Get the top member of the stack
shr1:
        srl H
        RR L
        push HL
        jp (IY)                 ;   

store_:                         ; Store the value at the address placed on the top of the stack
        pop HL               
        pop DE               
        ld (HL),E          
        inc HL              
        ld (HL),D          
        jp (IY)            
                                  
swap_:                      ; a b -- b a Swap the top 2 elements of the stack
        pop HL
        ex (SP),HL
        push HL
        jp (IY)
        
neg_:   
        ld HL, 0    		; NEGate the value on top of stack (2's complement)
        pop DE              ;    
        jr sub2             ; use the SUBtract routine
    
sub_:       		        ; Subtract the value 2nd on stack from top of stack 
        pop DE              ;    
        pop HL              ;      Entry point for INVert
sub2:   
        and A               ;      Entry point for NEGate
        sbc HL,DE           ; 15t
        push HL             ;    
        jp (IY)             ;   
                                ; 5  
eq_:    
        pop HL
        pop DE
        or A              ; reset the carry flag
        sbc HL,DE          ; only equality sets HL=0 here
        jr Z, true_
false_:
        ld HL, 0
        push HL
        jp (IY) 

gt_:    
        pop DE
        pop HL
        jr lt1
        
lt_:    
        pop HL
        pop DE
lt1:   
        or A              ; reset the carry flag
        sbc HL,DE         
	    jr Z,false_         
        jp M,false_
true_:
        ld HL, 1
        push HL
        jp (IY) 

gte_:    
        pop DE
        pop HL
        jr lte1
lte_:    
        pop HL
        pop DE
lte1:   
        or A              ; reset the carry flag
        sbc HL,DE         
        jp M,false_
        jp true


var_:
        ld A,(BC)
        call lookupRef2
        push HL
        jp (IY)
        
num_:   jp  num
lambda_:   
        jr lambda

a_:
        jp a
c_:
        jp c
d_:
        jp d
e_:
        jp e
f_:
        jp f
g_:
        jp g
i_:
        jp i
k_:
        jp k
l_:
        jp l
m_:
        jp m
n_:
        jp n
o_:
        jp o
p_:
        jp p
r_:
        jp r
s_:
        jp s
u_:
        jp u
w_:
        jp w
x_:
        jp x
        
div_:   jr div


;*******************************************************************
; Page 5 primitive routines 
;*******************************************************************
        ;falls through 

        push HL
        jp (IY)

lambda:                             ;=         
        inc BC
        ld DE,(vHeapPtr)            ; start of defintion
        push DE
lambda1:                                    ; Skip to end of definition   
        ld A,(BC)                   ; Get the next character
        inc BC                      ; Point to next character
        ld (DE),A
        inc DE
        cp ")"                      ; Is it a semicolon 
        jr NZ, lambda1              ; get the next element
lambda2:    
        dec BC
        ld (vHeapPtr),DE            ; bump heap ptr to after definiton
        jp (IY)       

; ********************************************************************
; 16-bit division subroutine.
;
; BC: divisor, DE: dividend, HL: remainder

; *********************************************************************            
; This divides DE by BC, storing the result in DE, remainder in HL
; *********************************************************************

; 1382 cycles
; 35 bytes (reduced from 48)
		

div:                                ;=34
        pop  DE                     ; get first value
        pop  HL                     ; get 2nd value
        push BC                     ; Preserve the IP
        ld B,H                      ; BC = 2nd value
        ld C,L		
		
        ld HL,0    	            ; Zero the remainder
        ld A,16    	            ; Loop counter

div1:		                    ;shift the bits from BC (numerator) into HL (accumulator)
        sla C
        rl B
        adc HL,HL

        sbc HL,DE		    ;Check if remainder >= denominator (HL>=DE)
        jr C,div2
        inc C
        jr div3
div2:		                    ; remainder is not >= denominator, so we have to add DE back to HL
        add hl,de
div3:
        dec A
        jr NZ,div1
        ld D,B                      ; Result from BC to DE
        ld E,C
div4:    
        pop  BC                     ; Restore the IP
        push DE                     ; push Result
        push HL                     ; push remainder             

        jp (IY)

        	                    ;=57                     

; **************************************************************************
; Page 6 Alt primitives
; **************************************************************************
        .align $100
page6:

anop_:
        jp (IY)                    

cFetch_:
        pop     HL          
        ld      D,0            
        ld      E,(HL)         
        push    DE              
        jp (IY)           
  
comment_:
        inc BC                      ; point to next char
        ld A,(BC)
        cp "\r"                     ; terminate at cr 
        jr NZ,comment_
        dec BC
        jp   (IY) 

cStore_:	  
        pop    HL               
        pop    DE               
        ld     (HL),E          
        jp     (IY)            
                             
emit_:
        pop HL
        ld A,L
        call putchar
        jp (IY)

exec_:
        call exec1
        jp (IY)
exec1:
        pop HL
        ex (SP),HL
        jp (HL)

prompt_:
        call prompt
        jp (IY)


go_:				                ;\^
        pop DE
go1:
        ld A,D                      ; skip if destination address is null
        or E
        jr Z,go3
        ld HL,BC
        inc BC                      ; read next char from source
        ld A,(BC)                   ; if ; to tail call optimise
        cp ";"                      ; by jumping to rather than calling destination
        jr Z,go2
        call rpush                  ; save Instruction Pointer
go2:
        ld BC,DE
        dec BC
go3:
        jp (IY)                     

inPort_:			    ; \<
        pop HL
        ld A,C
        ld C,L
        in L,(C)
        ld H,0
        ld C,A
        push HL
        jp (IY)        

newln_:
        call crlf
        jp (IY)        

outPort_:
        pop HL
        ld E,C
        ld C,L
        pop HL
        out (C),L
        ld C,E
        jp (IY)        

prnStr_:
prnStr:
        pop HL
        call putStr
        jp (IY)


rpush_:
        pop HL
        call rpush
        jp (IY)

rpop_:
        call rpop
        push HL
        jp (IY)

; **************************************************************************
; Page 6 primitive routines continued  (page 7) 
; **************************************************************************
        ; falls through to following page
a:
        inc BC
        ld A,(BC)
        cp 'd'              
        jp Z,add_
        cp 'n'              
        jp Z,and_
        dec BC
        jp var_
        
c:        
        inc BC
        ld A,(BC)
        cp 'a'              
        jp Z,case_
        cp 'l'              
        jp Z,closure_
        dec BC
        jp var_
        
d:        
        inc BC
        ld A,(BC)
        cp 'e'              
        jp Z,def_
        cp 'i'              
        jp Z,div_
        cp 'r'              
        jp Z,drop_
        cp 'u'              
        jp Z,dup_
        dec BC
        jp var_

e:
        inc BC
        ld A,(BC)
        cp 'q'              
        jp Z,eq_
        dec BC
        jp var_

f:
        inc BC
        ld A,(BC)
        cp 'i'              
        jp Z,filter_
        dec BC
        jp var_

g:
        inc BC
        ld A,(BC)
        cp 'e'              
        jp Z,get_
        cp 'o'              
        jp Z,go_
        cp 't'              
        jp Z,gt_
        dec BC
        jp var_

i:
        inc BC
        ld A,(BC)
        cp 'f'              
        jp Z,if_
        cp 'n'              
        jp Z,inv_
        dec BC
        jp var_

k:
        jp x
        inc BC
        ld A,(BC)
        cp 'e'              
        jp Z,key_
        dec BC
        jp var_

l:
        inc BC
        ld A,(BC)
        cp 'e'              
        jp Z,let_
        cp 't'              
        jp Z,lt_
        dec BC
        jp var_

m:
        inc BC
        ld A,(BC)
        cp 'a'              
        jp Z,map_
        cp 'u'              
        jp Z,mul_
        dec BC
        jp var_

n:
        inc BC
        ld A,(BC)
        cp 'e'              
        jp Z,neg_
        dec BC
        jp var_

o:
        inc BC
        ld A,(BC)
        cp 'v'              
        jp Z,over_
        cp 'r'              
        jp Z,or_
        dec BC
        jp var_

p:
        inc BC
        ld A,(BC)
        cp 'r'              
        jp Z,print_
        dec BC
        jp var_
r:
        inc BC
        ld A,(BC)
        cp 'o'              
        jp Z,rot_
        dec BC
        jp var_

s:
        inc BC
        ld A,(BC)
        cp 'c'              
        jp Z,scan_
        cp 'e'              
        jp Z,set_
        cp 'h'              
        jp Z,shift_
        cp 'u'              
        jp Z,sub_
        cp 'w'              
        jp Z,swap_
        dec BC
        jp var_

u:
        inc BC
        ld A,(BC)
        cp 'n'              
        jp Z,undrop_
        dec BC
        jp var_

w:
        inc BC
        ld A,(BC)
        cp 'h'              
        jp Z,while_
        dec BC
        jp var_

x:
        inc BC
        ld A,(BC)
        cp 'x'              
        jp Z,xor_
        dec BC
        jp var_

case_:
closure_:
def_:
filter_:
get_:
if_:
let_:
map_:
print_:
scan_:
set_:
shift_:
undrop_:
while_:

        jp (IY)

;*******************************************************************
; Page 5 primitive routines continued
;*******************************************************************

; ********************************************************************
; 16-bit multiply  
mul:                                ;=19
        pop  DE                     ; get first value
        pop  HL
        push BC                     ; Preserve the IP
        ld B,H                      ; BC = 2nd value
        ld C,L
        
        ld HL,0
        ld A,16
mul2:
        add HL,HL
        rl E
        rl D
        jr NC,$+6
        add HL,BC
        jr NC,$+3
        inc DE
        dec A
        jr NZ,mul2
		pop BC			    ; Restore the IP
		push HL                     ; Put the product on the stack - stack bug fixed 2/12/21
		jp (IY)

; ********************************************************************************
; Number Handling Routine - converts numeric ascii string to a 16-bit number in HL
; Read the first character. 
;			
; Number characters ($30 to $39) are converted to digits by subtracting $30
; and then added into the L register. (HL forms a 16-bit accumulator)
; Fetch the next character, if it is a number, multiply contents of HL by 10
; and then add in the next digit. Repeat this until a non-number character is 
; detected. add in the final digit so that HL contains the converted number.
; push HL onto the stack and proceed to the dispatch routine.
; ********************************************************************************
         
num:                                ;=23
		ld HL,$0000				    ;     Clear HL to accept the number
		ld A,(BC)				    ;     Get the character which is a numeral
        
num1:                               ; corrected KB 24/11/21

        sub $30                     ;       Form decimal digit
        add A,L                     ;       add into bottom of HL
        ld  L,A                     ;   
        ld A,00                     ;       Clear A
        adc	A,H	                    ; add with carry H-reg
	    ld	H,A	                    ; Put result in H-reg
      
        inc BC                      ;       Increment IP
        ld A, (BC)                  ;       and get the next character
        cp $30                      ;       Less than $30
        jr C, num2                  ;       Not a number / end of number
        cp $3A                      ;       Greater or equal to $3A
        jr NC, num2                 ;       Not a number / end of number
                                    ; Multiply digit(s) in HL by 10
        add HL,HL                   ;        2X
        ld  E,L                     ;        ld DE,HL
        ld  D,H                     ;    
        add HL,HL                   ;        4X
        add HL,HL                   ;        8X
        add HL,DE                   ;        2X  + 8X  = 10X
                                    ; 52t cycles

        jr  num1
                
num2:
        dec BC
        push HL                     ; Put the number on the stack
        jp (IY)                     ; and process the next character

hex:                                ;=26
	ld HL,0	    		    ; Clear HL to accept the number
hex1:
        inc BC
        ld A,(BC)		    ; Get the character which is a numeral
        bit 6,A                     ; is it uppercase alpha?
        jr Z, hex2                  ; no a decimal
        sub 7                       ; sub 7  to make $A - $F
hex2:
        sub $30                     ; Form decimal digit
        jp C,num2
        cp $0F+1
        jp NC,num2
        add HL,HL                   ; 2X ; Multiply digit(s) in HL by 16
        add HL,HL                   ; 4X
        add HL,HL                   ; 8X
        add HL,HL                   ; 16X     
        add A,L                     ; add into bottom of HL
        ld  L,A                     ;   
        jr  hex1

;*******************************************************************
; Subroutines
;*******************************************************************

prompt:                             ;=9
        call printStr
        .cstr "\r\n> "
        ret

putStr0:
        call putchar
        inc HL
putStr:
        ld A,(HL)
        or A
        jr NZ,putStr0
        ret

rpush:                              ;=11
        dec IX                  
        ld (IX+0),H
        dec IX
        ld (IX+0),L
        ret

rpop:                               ;=11
        ld L,(IX+0)         
        inc IX              
        ld H,(IX+0)
        inc IX                  
rpop2:
        ret

crlf:                               ;=7
        call printStr
        .cstr "\r\n"
        ret

