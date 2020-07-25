;*****************start of the kernel code***************
[org 0x000]
[bits 16]

[SEGMENT .text]

;START #####################################################
    mov ax, 0x0100			;location where kernel is loaded
    mov ds, ax
    mov es, ax
    
    cli
    mov ss, ax				;stack segment
    mov sp, 0xFFFF			;stack pointer at 64k limit
    sti

    push dx
    push es
    xor ax, ax
    mov es, ax
    cli
    mov word [es:0x21*4], _int0x21	; setup interrupt service
    mov [es:0x21*4+2], cs
    sti
    pop es
    pop dx

    mov si, strWelcomeMsg   ; load message
    mov al, 0x01            ; request sub-service 0x01
    int 0x21

	call _shell				; call the shell
    
    int 0x19                ; reboot
;END #######################################################

_int0x21:
    _int0x21_ser0x01:       ;service 0x01
    cmp al, 0x01            ;see if service 0x01 wanted
    jne _int0x21_end        ;goto next check (now it is end)
    
	_int0x21_ser0x01_start:
    lodsb                   ; load next character
    or  al, al              ; test for NUL character
    jz  _int0x21_ser0x01_end
    mov ah, 0x0E            ; BIOS teletype
    mov bh, 0x00            ; display page 0
    mov bl, 0x07            ; text attribute
    int 0x10                ; invoke BIOS
    jmp _int0x21_ser0x01_start
    _int0x21_ser0x01_end:
    jmp _int0x21_end

    _int0x21_end:
    iret

_shell:
	_shell_begin:
	;move to next line
	call _display_endl

	;display prompt
	call _display_prompt

	;get user command
	call _get_command
	
	;split command into components
	call _split_cmd

	;check command & perform action

	; empty command
	_cmd_none:		
	mov si, strCmd0
	cmp BYTE [si], 0x00
	jne	_cmd_ver		;next command
	jmp _cmd_done
	
	; display version
	_cmd_ver:		
	mov si, strCmd0
	mov di, cmdVer
	mov cx, 4
	repe	cmpsb
	jne	_cmd_list		;next command
	
	call _display_endl
	mov si, strOsName		;display version
	mov al, 0x01
    int 0x21
	call _display_space
	mov si, txtVersion		;display version
	mov al, 0x01
    int 0x21
	call _display_space

	mov si, strMajorVer		
	mov al, 0x01
    int 0x21
	mov si, strMinorVer
	mov al, 0x01
    int 0x21
	jmp _cmd_done

    _cmd_list:
	mov si, strCmd0;load the input command
	mov di, cmdInfo;compare if the command is matching with the one you provided
	mov cx, 6
	repe cmpsb
	jne _cmd_HardInfo  ;next command
	
	call _display_cmd_List;call the subroutine that prints the help commands
	jmp _cmd_done

	_cmd_HardInfo:
	mov si, strCmd0;load the input command
	mov di, cmdHardInfo;compare if the command is matching with the one you provided
	mov cx, 8
	repe	cmpsb
	jne	_cmd_status		;next command
	call    _hardinf;call the subroutine that displays the hardware info
	
	jmp _cmd_done
	
    _cmd_status:
        mov si, strCmd0
	mov di, cmdBootInfo
	mov cx, 8
	repe	cmpsb
	jne	_cmd_exit		;next command
	call    _boot_status
	jmp _cmd_done

	; exit shell
	_cmd_exit:		
	mov si, strCmd0
	mov di, cmdExit
	mov cx, 5
	repe	cmpsb
	jne	_cmd_unknown		;next command

	je _shell_end			;exit from shell

	_cmd_unknown:
	call _display_endl
	mov si, msgUnknownCmd		;unknown command
	mov al, 0x01
    int 0x21

	_cmd_done:

	;call _display_endl
	jmp _shell_begin
	
	_shell_end:
	ret

_get_command:
	;initiate count
	mov BYTE [cmdChrCnt], 0x00
	mov di, strUserCmd

	_get_cmd_start:
	mov ah, 0x10		;get character
	int 0x16

	cmp al, 0x00		;check if extended key
	je _extended_key
	cmp al, 0xE0		;check if new extended key
	je _extended_key

	cmp al, 0x08		;check if backspace pressed
	je _backspace_key

	cmp al, 0x0D		;check if Enter pressed
	je _enter_key

	mov bh, [cmdMaxLen]		;check if maxlen reached
	mov bl, [cmdChrCnt]
	cmp bh, bl
	je	_get_cmd_start

	;add char to buffer, display it and start again
	mov [di], al			;add char to buffer
	inc di					;increment buffer pointer
	inc BYTE [cmdChrCnt]	;inc count

	mov ah, 0x0E			;display character
	mov bl, 0x07
	int 0x10
	jmp	_get_cmd_start

	_extended_key:			;extended key - do nothing now
	jmp _get_cmd_start

	_backspace_key:
	mov bh, 0x00			;check if count = 0
	mov bl, [cmdChrCnt]
	cmp bh, bl
	je	_get_cmd_start		;yes, do nothing
	
	dec BYTE [cmdChrCnt]	;dec count
	dec di

	;check if beginning of line
	mov	ah, 0x03		;read cursor position
	mov bh, 0x00
	int 0x10

	cmp dl, 0x00
	jne	_move_back
	dec dh
	mov dl, 79
	mov ah, 0x02
	int 0x10

	mov ah, 0x09		; display without moving cursor
	mov al, ' '
    mov bh, 0x00
    mov bl, 0x07
	mov cx, 1			; times to display
    int 0x10
	jmp _get_cmd_start

	_move_back:
	mov ah, 0x0E		; BIOS teletype acts on backspace!
    mov bh, 0x00
    mov bl, 0x07
    int 0x10
	mov ah, 0x09		; display without moving cursor
	mov al, ' '
    mov bh, 0x00
    mov bl, 0x07
	mov cx, 1			; times to display
    int 0x10
	jmp _get_cmd_start

	_enter_key:
	mov BYTE [di], 0x00
	ret

_split_cmd:
	;adjust si/di
	mov si, strUserCmd
	;mov di, strCmd0

	;move blanks
	_split_mb0_start:
	cmp BYTE [si], 0x20
	je _split_mb0_nb
	jmp _split_mb0_end

	_split_mb0_nb:
	inc si
	jmp _split_mb0_start

	_split_mb0_end:
	mov di, strCmd0

	_split_1_start:			;get first string
	cmp BYTE [si], 0x20
	je _split_1_end
	cmp BYTE [si], 0x00
	je _split_1_end
	mov al, [si]
	mov [di], al
	inc si
	inc di
	jmp _split_1_start

	_split_1_end:
	mov BYTE [di], 0x00

	;move blanks
	_split_mb1_start:
	cmp BYTE [si], 0x20
	je _split_mb1_nb
	jmp _split_mb1_end

	_split_mb1_nb:
	inc si
	jmp _split_mb1_start

	_split_mb1_end:
	mov di, strCmd1

	_split_2_start:			;get second string
	cmp BYTE [si], 0x20
	je _split_2_end
	cmp BYTE [si], 0x00
	je _split_2_end
	mov al, [si]
	mov [di], al
	inc si
	inc di
	jmp _split_2_start

	_split_2_end:
	mov BYTE [di], 0x00

	;move blanks
	_split_mb2_start:
	cmp BYTE [si], 0x20
	je _split_mb2_nb
	jmp _split_mb2_end

	_split_mb2_nb:
	inc si
	jmp _split_mb2_start

	_split_mb2_end:
	mov di, strCmd2

	_split_3_start:			;get third string
	cmp BYTE [si], 0x20
	je _split_3_end
	cmp BYTE [si], 0x00
	je _split_3_end
	mov al, [si]
	mov [di], al
	inc si
	inc di
	jmp _split_3_start

	_split_3_end:
	mov BYTE [di], 0x00

	;move blanks
	_split_mb3_start:
	cmp BYTE [si], 0x20
	je _split_mb3_nb
	jmp _split_mb3_end

	_split_mb3_nb:
	inc si
	jmp _split_mb3_start

	_split_mb3_end:
	mov di, strCmd3

	_split_4_start:			;get fourth string
	cmp BYTE [si], 0x20
	je _split_4_end
	cmp BYTE [si], 0x00
	je _split_4_end
	mov al, [si]
	mov [di], al
	inc si
	inc di
	jmp _split_4_start

	_split_4_end:
	mov BYTE [di], 0x00

	;move blanks
	_split_mb4_start:
	cmp BYTE [si], 0x20
	je _split_mb4_nb
	jmp _split_mb4_end

	_split_mb4_nb:
	inc si
	jmp _split_mb4_start

	_split_mb4_end:
	mov di, strCmd4

	_split_5_start:			;get last string
	cmp BYTE [si], 0x20
	je _split_5_end
	cmp BYTE [si], 0x00
	je _split_5_end
	mov al, [si]
	mov [di], al
	inc si
	inc di
	jmp _split_5_start

	_split_5_end:
	mov BYTE [di], 0x00

	ret

_display_space:
	mov ah, 0x0E                            ; BIOS teletype
	mov al, 0x20
    mov bh, 0x00                            ; display page 0
    mov bl, 0x07                            ; text attribute
    int 0x10                                ; invoke BIOS
	ret

_display_endl:
	mov ah, 0x0E		; BIOS teletype acts on newline!
    mov al, 0x0D
	mov bh, 0x00
    mov bl, 0x07
    int 0x10
	mov ah, 0x0E		; BIOS teletype acts on linefeed!
    mov al, 0x0A
	mov bh, 0x00
    mov bl, 0x07
    int 0x10
	ret

_display_prompt:
	mov si, strPrompt
	mov al, 0x01
	int 0x21
	ret

_hardinf:
	call _display_endl
	_display_serial_ports:
		call _display_endl
		
		mov si, strSerial
			mov al, 0x01
			int 0x21
			
		xor ax, ax;clear the ax register
		int 0x11;send the interupt to get data
		and ax, 0xe00;get the bit for serial ports
		shr ax, 9;shift the corresponding bit to get the corresponding bit value
		add ax, 48;add 48 to convert to string representation of decimal value
		mov ah, 0x0e;BIOS teletype output
		int 0x10;send the BIOS output interrupt
		 
		
	_video_mode:
		call _display_endl
		
		mov si, strVideo
			mov al, 0x01
			int 0x21
			
		xor ax, ax;clear the ax register
		int 0x11;send the interupt to get data
		and ax, 0x020;get the bit for video mode
		shr ax, 5;shift the corresponding bit to get the corresponding bit value
		add ax, 48;add 48 to convert to string representation of decimal value
		mov ah, 0x0e
		int 0x10

		call _display_endl
		mov si, strUnused
			mov al, 0x01
			int 0x21
		call _display_endl
		mov si, strColor1
			mov al, 0x01
			int 0x21
		call _display_endl
		mov si, strColor2
			mov al, 0x01
			int 0x21
		call _display_endl
		mov si, strMchrome
			mov al, 0x01
			int 0x21
		
		

	_display_printer_ports:
		call _display_endl
		
		mov si, strPrinter
			mov al, 0x01
			int 0x21
			
		xor ax, ax;clear the ax register
		int 0x11;send the interupt to get data
		and ax, 0xc000;get the bit for printer ports
		shr ax, 14;shift the corresponding bit to get the corresponding bit value
		add ax, 48;add 48 to convert to string representation of decimal value
		mov ah, 0x0e;BIOS teletype output
		int 0x10;send the BIOS output interrupt
		 

	_check_dma:
		call _display_endl
		
		mov si, strDma
			mov al, 0x01
			int 0x21
			
		xor ax, ax;clear the ax register
		int 0x11;send the interupt to get data
		and ax, 0x100;get the bit for dma 
		shr ax, 8;shift the corresponding bit to get the corresponding bit value 
		add ax, 48;add 48 to convert to string representation of decimal value
		mov ah, 0x0e;BIOS teletype output
		int 0x10;send the BIOS output interrupt
		 

	_check_pointing_device:
		call _display_endl
		
		mov si, strPointing
			mov al, 0x01
			int 0x21
			
		xor ax, ax;clear the ax register
		int 0x11;send the interupt to get data
		and ax, 0x004;get the bit for pointing device
		shr ax, 2;shift the corresponding bit to get the corresponding bit value
		add ax, 48;add 48 to convert to string representation of decimal value
		mov ah, 0x0e;BIOS teletype output
		int 0x10;send the BIOS output interrupt
		 
		
		
	_diskette:
		call _display_endl
		mov si, strDiskette
		mov al, 0x01
		int 0x21
		xor ax, ax;clear the ax register
		int 0x11;send the interupt to get data
		and ax, 0x80;get the bit for diskette
		shr ax, 6;shift the corresponding bit to get the corresponding bit value
		add ax, 48;add 48 to convert to string representation of decimal value
		mov ah, 0x0e;BIOS teletype output
		int 0x10;send the BIOS output interrupt
		
		

	_hard_drive:
		call _display_endl

			mov si, strHardDrive
			mov al, 0x01
			int 0x21

			mov ax, 0x0040;set BIOS data area address 
			push es;save es for later use
			mov es,ax;copy the value of AX to ES
			mov al,[es:0x0075]	; read 40:75 
			add al, 48;add 48 to convert to string representation of decimal value
			pop es;restore ES value
			mov ah, 0x0e;BIOS teletype output
			int 0x10;send the BIOS output interrupt
			

			
	_bios_date:
			call _display_endl
			mov si, strBios
			mov al, 0x01
			int 0x21
			push es
			mov ax, 0xf000		;BIOS release date is saved in F000:FFF5
			mov es, ax
			mov si, 0xfff5
			mov bl,8
			_loop:
				mov al, [es:si]
				mov ah, 0x0e;BIOS teletype output
				int 0x10;send the BIOS output interrupt
				inc si
				dec bl
				cmp bl, 0
				jne _loop
			pop es
			

		
		
	_get_cpuid:
		_cmd_cpuVendorID:
		call _display_endl
		
		mov si, strVendor
			mov al, 0x01
			int 0x21
		mov eax,0

		cpuid; call cpuid command
		mov [strcpuid],ebx; load last string
		mov [strcpuid+4],edx; load middle string
		mov [strcpuid+8],ecx; load first string
		mov si, strcpuid	; Put string position into SI
		call _disp_str    


	_cmd_ProcessorType:
		mov eax,0x80000002
		cpuid     ; call cpuid command
		mov [strcputype]   ,eax;
		mov [strcputype+4] ,ebx
		mov [strcputype+8] ,ecx
		mov [strcputype+12],edx

		mov eax,0x80000003
		cpuid; call cpuid command

		mov [strcputype+16],eax
		mov [strcputype+20],ebx
		mov [strcputype+24],ecx
		mov [strcputype+28],edx

		call _display_endl
		mov si, strCpuType
			mov al, 0x01
			int 0x21

		mov eax,0x80000004
		cpuid     ; call cpuid command
		mov [strcputype+32],eax
		mov [strcputype+36],ebx
		mov [strcputype+40],ecx
		mov [strcputype+44],edx
		mov si, strcputype	; Put string position into SI
		call _disp_str	; Call our string-printing routine
		
		
		
	_show_memory_info:
	    	;*******display lower memory size
	    	;read the offset 13h of the 0040:0000h to get the Memory size in KB
	    	
	    	call _display_endl
	    	mov si, strLowerMemory
	    	mov al, 0x01
	    int 0x21
	    	
	    	call _display_space
		
		push es
		mov ax, 0x40
		mov es, ax
		mov ax, [es:13h]	;13h offset from 0040:0000h includes the lower memory size
		mov dx, ax
		call _display_space
		call _hex2dec
		mov si, strKB
		mov al, 0x01
	    int 0x21
		
		pop es
	    	
	    	call _display_endl
	    	
	    	xor cx, cx			;clear cx
		xor dx, dx			;clear dx
		mov ax, 0xe801
	    int 0x15
		jc _MemErr		; CF set on error
		cmp ah, 0x86		; unsupported function
		je _MemErr
		cmp ah, 0x80		; invalid command
		je _MemErr

		mov si, strUpperMemory
		mov al, 0x01
	    int 0x21
		
		call _display_space
		
		cmp cx, 0x0000		;if CX=0
		je _remove_cx_conflict
		jmp _memCalculate

	_remove_cx_conflict:		
		;some bioses return CX=BX=0 if so copy ax to cx and bx to dx
		mov cx,ax		
		mov dx,bx

	_memCalculate:
		;Now CX = configured memory 1M to 16M, in K
		;Now DX = configured memory above 16M, in 64K blocks
		;configured memory above 16M in MBs = DX*64/1024 = (DX/2^4)
		shr dx, 4		;divide dx by 2^4 or shift 4 bits right
		shr cx, 10		;divide cx by 2^10 or shift 10 bits right
		add cx,dx		;total memory

		mov dx, cx		;move total memory size to dx
		call _hex2dec		;convert hex to decimal	
		mov si, strMB
		mov al, 0x01
	    int 0x21
		call _display_endl
		ret
			
	_MemErr:
		mov si, strMemError	;in case an error occured while reading memory
		mov al, 0x01
	    int 0x21
		ret
	;end
	
		

	



_boot_status:  
	call _display_endl
	_casplock_status_pressed:
	    
	    push es ;save the current values of the es register
	    mov ax, 0x40 ;get the address no 0x40 to the ax register
	    mov es, ax ;move eax to the es
	    mov al, [es:17h] ;get the required offset to display no of HDD
	    and al, 0x40
	    shr al, 6
	    add al, 0x30               ;add 48 and convert to decimal
	    
	    mov ah, 0x0E                            ; BIOS teletype
	    mov bh, 0x00                            ; display page 0
	    mov bl, 0x07                            ; text attribute
	    int 0x10                                ; invoke BIOS
	    pop es ;restore the value stored in the es register

	_capslock_status_pressing:
	    
	    push es ;save the current values of the es register
	    mov ax, 0x40 ;get the address no 0x40 to the ax register
	    mov es, ax ;move eax to the es
	    mov al, [es:18h] ;get the required offset to display no of HDD
	    and al, 0x40
	    shr al, 6
	    add al, 0x30               ;add 48 and convert to decimal 
	    mov ah, 0x0E                            ; BIOS teletype
	    mov bh, 0x00                            ; display page 0
	    mov bl, 0x07                            ; text attribute
	    int 0x10                                ; invoke BIOS
	    pop es ;restore the value stored in the es register
	    
	    ret
    
    
_display_cmd_List:
		call _display_endl
		call _display_endl
		mov si, strList
		mov al, 0x01
		int 0x21
		call _display_endl
		mov si, strVer
		mov al, 0x01
		int 0x21
		call _display_endl
		mov si, strHard
		mov al, 0x01
		int 0x21
		call _display_endl
		mov si, strReboot
		mov al, 0x01
		int 0x21
		call _display_endl
			
		ret

    
  
 
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
_disp_str:
    lodsb                                   ; load next character
    or  al, al                              ; test for NUL character
    jz  .DONE
    mov ah, 0x0E                            ; BIOS teletype
    mov bh, 0x00                            ; display page 0
    mov bl, 0x07                            ; text attribute
    int 0x10                                ; invoke BIOS
    jmp _disp_str
.DONE:
    ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
_print_dec:
	push ax			; save AX
	push cx			; save CX
	push si			; save SI
	mov ax,dx		; copy number to AX
	mov si,10		; SI is used as the divisor
	xor cx,cx		; clear CX

_non_zero:

	xor dx,dx		; clear DX
	div si			; divide by 10
	push dx			; push number onto the stack
	inc cx			; increment CX to do it more times
	or ax,ax		; clear AX
	jne _non_zero		; if not go to _non_zero

_prepare_digits:

	pop dx			; get the digit from DX
	add dl,0x30		; add 30 to get the ASCII value
	call _print_char	; print char
	loop _prepare_digits	; loop till cx == 0

	pop si			; restore SI
	pop cx			; restore CX
	pop ax			; restore AX
	ret                      

_print_char:
	push ax			; save AX 
	mov al, dl
        mov ah, 0x0E		; BIOS teletype acts on printing char
        mov bh, 0x00
        mov bl, 0x07
        int 0x10

	pop ax			; restore AX
	ret
    
    
    
_hex2dec:
	;converts the number in dx to decimal and print it
	push ax
	push bx
	push cx
	push si
	mov ax,dx                ; copy number into AX
	mov si,10                ; SI will be our divisor
	xor cx,cx                ; clean up the CX

_nonzero:
	xor dx,dx                ; clear DX
	div si                   ; divide by 10
	push dx                  ; push number onto the stack
	inc cx                   ; increment CX to do it more times
	or ax,ax                 ; end of the number?
	jne _nonzero             ; if not repeat

_write_digits:
	pop dx                   ; get the digit off DX
	add dl,48                ; add 48 to get ASCII
	mov al, dl
	mov ah, 0x0e
	int 0x10
	loop _write_digits


	pop si
	pop cx
	pop bx
	pop ax
	ret
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


[SEGMENT .data]
        strWelcomeMsg          db  "Welcome to JOSH Version 0.04 edited by Neminda Prabhashwara", 0x00
	strPrompt		db	"JOSH>>", 0x00
	strInfo			db	"You can view all available commands by entering lscmd",0x00
	strVer			db	"ver     : to view the version of the operating system",0x00
	strList			db	"lscmd   : to view available commands",0x00
	strHard			db	"hardinf : to get hardware information",0x00
	strReboot		db	"exit    : reboot",0x00
	
	cmdMaxLen		db	255			;maximum length of commands

	strOsName		db	"JOSH", 0x00	;OS details
	strMajorVer		db	"0", 0x00
	strMinorVer		db	".04", 0x00
	strMemError 	        db	"Memory detect error",0x00
	strUpperMemory		db	"Total Upper Memory               : ",0x00
	strLowerMemory         db	"Total Lower Memory               : ",0x00
	strMB			db	" MB",0x00
	strKB                  db      " KB",0x00
	strHardDrive	        db	"Hard drives installed      : ",0x00
	strSerial 		db	"Serial ports available     : ",0x00
	strVideo 		db	"Initial video mode         : ",0x00
	strUnused		db	"	0--Unused",0x00
	strColor1		db	"	1--40x25 color",0x00
	strColor2		db	"	2--80x25 color",0x00
	strMchrome		db	"	3--80x25 monochrome",0x00
	strPrinter		db	"Printer ports available    : ",0x00
	strDma			db	"DMA installed(if yes, 0)   : ",0x00
	strPointing		db	"Pointing device installed  : ",0x00
	strDiskette		db	"Floppy drives available    : ",0x00
	strVendor		db	"CPU vendor                 : ",0x00
	strCpuType		db	"CPU type                   : ",0x00
	strBios		db	"Bios release date          : ",0x00
	strcapspressed         db      "Capslock is pressed        : ",0x00

	cmdVer			db	"ver", 0x00		; internal commands
	cmdExit		db	"exit", 0x00
	cmdHardInfo		db	"hardinf", 0x00
	cmdBootInfo		db	"bootinf", 0x00
	cmdDiskette		db	"floppy", 0x00
	cmdInfo	        db	"lscmd", 0x00
	

	txtVersion		db	"version", 0x00	;messages and other strings
	msgUnknownCmd	        db	"Unknown command or bad file name!", 0x00

[SEGMENT .bss]
	strUserCmd	        resb	256		;buffer for user commands
	cmdChrCnt	        resb	1		;count of characters
	strCmd0		resb	256		;buffers for the command components
	strCmd1		resb	256
	strCmd2		resb	256
	strCmd3		resb	256
	strCmd4		resb	256
	strcpuid               resb    255 
	strcputype             resb    255 

;********************end of the kernel code********************

