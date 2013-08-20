 title Global Environment Support
 subttl Prologue
 page 54,120
;*=====================================================================*
;*                                                                     *
;*                            G L O B E N V                            *
;*                                                                     *
;*                        Dynamic Link Routines                        *
;*                                 for                                 *
;*                     Global Environment Support                      *
;*                                                                     *
;* The Global Environment is a store of named data items that is       *
;* accessible to any process in an OS/2 system.  Data items are stored *
;* and retrieved by name, where a name is an AsciiZ string.  The value *
;* of a data item is any sequence of bytes assigned to it (usually,    *
;* but not necessarily, another AsciiZ string).                        *
;*                                                                     *
;* The following routines are exported by this package.  See procedure *
;* headings for more detail; see also the documentation file.          *
;*                                                                     *
;* GlbEnter(in:name)                                                   *
;*      defines a new name, or discovers if a name is defined.         *
;*                                                                     *
;* GlbStore(in:name, value, length) -- assign a value of a given       *
;*      length to a predefined name, replacing any value it has now.   *
;*                                                                     *
;* GlbFetch(in:name; out: value; inout:length)                         *
;*      copies the value of a name, not to exceed a specified length,  *
;*      and reports back the length copied.                            *
;*                                                                     *
;* GlbQuery(in:name; out:length)                                       *
;*      discovers if a name exists, and the length of its value.       *
;*                                                                     *
;* GlbNext(in:name, strmax; out: string, size)                         *
;*      returns the name-string lexically next-greater than the input  *
;*      name (provided it fits in strmax bytes), and the length of     *
;*      its value if any.  in:name and out:string may be the same      *
;*      space and thus the whole set of names may be scanned in order. *
;*                                                                     *
;* GlbDelete(in:name)                                                  *
;*      deletes a name and its value if any.                           *
;*                                                                     *
;* These names (in UPPER CASE) are exported for dynamic linking. See   *
;* the documentation file for details of declaration in C or Pascal.   *
;*                                                                     *
;*  Copyright (C) 1987 David E. Cortesi                                *
;*                                                                     *
;* History:                                                            *
;* 10/1/87 -- begun                                                    *
;* 10/6/87 -- substantially complete                                   *
;* 10/12/87 -- start-counter, RdrCount/WtrCount underflow prevention   *
;* 10/13/87 -- test Xtend, Gcoll and fix bugs therein                  *
;* 10/15/87 -- add Exit List routine                                   *
;*                                                                     *
;*=====================================================================*
        .286P
; above should be two lines, .286 and .PRIV, but that don't work in SDK
        public  GLBENTER
        public  GLBSTORE
        public  GLBQUERY
        public  GLBFETCH
        public  GLBNEXT
        public  GLBDELETE

        extrn  DosSemSet:far
        extrn  DosSemClear:far
        extrn  DosSemRequest:far
        extrn  DosSemWait:far
        extrn  DosReallocSeg:far
        extrn  DosExitList:far
        extrn  DosEnterCritSec:far
        extrn  DosExitCritSec:far

 subttl Global Data Area
 page
;*=====================================================================*
;*                                                                     *
;* The package has a single data segment which is solo, common to all  *
;* processes that link this code.  All public procedures use their     *
;* caller's stack but load up the selector for this segment into DS    *
;* on entry.  Inner routines all "assume ds:GLOBDATA."                 *
;*                                                                     *
;* GLOBDATA is laid out this way, from low offsets to higher ones:     *
;*                                                                     *
;*      * fixed, defined fields use to manage the segment              *
;*              * semaphores for exclusion                             *
;*              * offsets and counts for storage management            *
;*                                                                     *
;*      * a "heap" of "objects" of variable size                       *
;*              * every object has an integral number of words         *
;*              * the first word is always the object's byte length    *
;*              * the second word is its Control index (later)         *
;*              * then follows its contents, one or more words         *
;*                                                                     *
;*      * zero or more free bytes                                      *
;*                                                                     *
;*      * an array of 4*N words, where N is the highest number of      *
;*        names ever defined.  Initially N=0.                          *
;*              * array entries are addressed *backward* from the      *
;*                size of the segment: word K is at (SegLimit - 2K).   *
;*                This permits the segment to be extended at any time. *
;*                                                                     *
;* There are a number of (fairly obvious) relationships among these    *
;* values that are assumed to be true by the using code.  However,     *
;* OS/2 gives us the ability to detect when a predecessor crashed (see *
;* the ReqCtrl procedure) and we take that as the occasion to make     *
;* explicit integrity checks of the contents of the segment (see the   *
;* Integrity procedure).  The same procedure can also be called on     *
;* every entry or every Writer exit (see ReqCtrl, WtrOut).             *
;*                                                                     *
;*=====================================================================*
GLOBDATA  segment para public 'DATA'
; error codes
Err_no_name     equ     1
Err_name_exists equ     2
Err_no_room     equ     3
Err_trunc_val   equ     4

; semaphores and counters for exclusion, see RdrIn proc and following.
CtrlSem         dd      0       ; Key sem for exclusive write access
WtrWait         dd      0       ; Signal to writers, last reader done
RdrGate         dd      0       ; Signal to readers, last writer done
RdrCount        dw      0       ; When nonzero, readers at work
WtrCount        dw      0       ; When nonzero, writers waiting

; curiosity counter to see whether start is called once or often
StartCount      dw      0

; heap management items, see GColl proc and following
HeapBot         dw      EndFixed ; bottom of heap
HeapMid         dw      EndFixed ; top of in-use objects
HeapTop         dw      EndArray ; end of free space
HeapGarb        dw      0       ; bytes of garbage in heap

SegLimit        dw      EndArray ; size of segment currently

ArraySize       dw      0       ; number of groups of 4 words in array
ArrayFree       dw      0       ; number of them free (from deletes)

; This is the end of the fixed part of GLOBDATA.  Put any added fixed
; field above this point.  DO NOT re-open the definition of this seg.

EndFixed        equ     $
                db      2048 dup(?) ; initial heap and array
EndArray        equ     $

GLOBDATA        ends

 subttl Entry and Setup Formalities
 page
;*=====================================================================*
;*                                                                     *
;* The "start" proc receives control when the DLL is first loaded.     *
;* It doesn't need to do anything but return 1 for success, but it     *
;* also gets the exact size of the data segment.                       *
;*                                                                     *
;*=====================================================================*

GLOBENV segment para public 'CODE'
                assume  cs:GLOBENV, ds:GLOBDATA

  start proc far
                mov     ax, seg GLOBDATA
                mov     ds, ax
                lsl     ax, ax ; yields offset of last byte (odd number)
                jz      startOK ; segment exists!
                xor     ax, ax ; initialization failed
                jmp short startExit
startOK:        inc     StartCount
                test    StartCount, -2
                jnz     startExit ; don't init more than once
                inc     ax ; make length/offset-of-byte-after-seg
                mov     SegLimit, ax ; set hardware seg limit
                mov     ax,1 ; above inc could have made ax=0
startExit:      ret
  start endp

 subttl Exit List Items
 page
;*=====================================================================*
;*                                                                     *
;* We have to know when a client is terminating, in order to keep our  *
;* Rdr/WtrCounts straight.  We maintain a per-client data area and use *
;* it to note (1) whether we've set up an Exit routine for this client *
;* and (2) what the client's state is as a reader or writer.           *
;*                                                                     *
;* The following data segment is designated (in the GLOBENV.DEF file)  *
;* as "multiple," that is, per-process.  Each process that links this  *
;* package gets a unique copy of this segment.  It's small but useful. *
;*                                                                     *
;*=====================================================================*

PERCLIENT segment byte public 'DATA'
ActiveReader db 0 ; incremented for every thread of proc that's a reader
ActiveWriter db 0 ; 01 when a thread is an active writer
HaveExitList db 0 ; nonzero when an exit list established
PERCLIENT ends

; This subroutine returns the per-client switches in registers:
;       DH = ActiveReader  DL = ActiveWriter  AL = HaveExitList

    GetPerClient proc near
        push    es
        mov     ax, seg PERCLIENT
        mov     es, ax
        mov     dh, es:byte ptr ActiveReader
        mov     dl, es:byte ptr ActiveWriter
        mov     al, es:byte ptr HaveExitList
        pop     es
        ret
    GetPerClient endp

; This subroutine sets the per-client switches from registers as above.
; The use of the subroutines saves pushing/popping es in other code.

    PutPerClient proc near
        push    es
        push    ax
        mov     ax, seg PERCLIENT
        mov     es, ax
        pop     ax
        mov     es:byte ptr ActiveReader, dh
        mov     es:byte ptr ActiveWriter, dl
        mov     es:byte ptr HaveExitList, al
        pop     es
        ret
    PutPerClient endp

        page
;*=====================================================================*
;*                                                                     *
;*  Exit routine: this code gets control when a client process is      *
;* terminating.  If the client was an active reader or writer, we try  *
;* to clean up after.  Perfect reliability is not assured, since there *
;* are stretches of code over which termination could cause problems   *
;* but over which the relevant PerClient flags are not yet set.        *
;*                                                                     *
;*=====================================================================*

   ExitRoutine proc far
                mov     ax, seg GLOBDATA
                mov     ds, ax

                call    GetPerClient
                or      dl, dl          ; were we a writer?
                jz      ERCheckRdr      ; (no)

; The dying client was the (one and only) active writer task.  Call the
; Integrity routine to clear the workspace if it's damaged.  Then exit
; as a writer.

                call    Integrity
                call    WtrOut
                jmp short ERExit

ERCheckRdr:     or      dh, dh          ; were we one or more readers?
                jz      ERExit          ; (no)

; The expiring process was a reader.  In fact it could have been more
; than one reader, since it could have had multiple threads and we
; allow multiple readers.  But it's down to one thread now, so check
; out on behalf of all of them.

; We want to claim CtrlSem, but we don't want to hang the system for
; very long (Exit routines are supposed to be quick).  So we will use
; a timeout of 500L (half a second) on the SemRequest.

                push    ds
                push    offset CtrlSem
                push    0       ; push M.S. word first,
                push    500     ; ..and least-significant second
                call    DosSemRequest
                cmp     ax, 121 ; did wait end for timeout?
                je      ERExit  ; (yes, give up)
                mov     al, dh  ; number of reader threads
                cbw             ; ..in this process
                neg     ax
                add     ax, RdrCount
                mov     RdrCount, ax
                push    ds
                push    offset CtrlSem
                call    DosSemClear

; We don't need to, or can't, clean up, or are finished.  Exit to OS/2
; using DosExitList(3), transfer to next exit routine.
ERExit:         push    3
                push    0       ; push null routine address
                push    0
                call    DosExitList ; SHOULD NOT RETURN
                ret             ; (in case it does...)
    ExitRoutine endp

; This routine sets up the above code as an Exit routine for the
; process that is currently animating this code.
    SetExitList proc near
                push    ax
                push    1
                push    seg GLOBENV
                push    offset GLOBENV:ExitRoutine
                call    DosExitList
                pop     ax
                ret
    SetExitList endp

    subttl Procedures for Mutual Exclusion
    page
;*=====================================================================*
;*                                                                     *
;*  Define a Reader as a thread executing in GlbFetch, GlbQuery, or    *
;*  GlbNext.  These procedures don't alter any data in GLOBDATA.  Any  *
;*  number of them may run concurrently.                               *
;*                                                                     *
;*  Define a Writer as a thread executing in GlbEnter, GlbStore, or    *
;*  GlbDelete.  These procedures do alter the heap and the array.      *
;*  No other thread may be in this module while a Writer is running.   *
;*                                                                     *
;*  The simple solution would be to use one semaphore to serialize the *
;*  use of GLOBDATA. But for efficiency we would prefer to allow any   *
;*  number of readers when only readers are around, serializing only   *
;*  when a writer is running.                                          *
;*                                                                     *
;*  The full solution used here, including the use of Exit Routines    *
;*  and the handling of the per-client data area, is discussed in the  *
;*  text.  The pseudo-code precedes the main routines that follow.     *
;*                                                                     *
;*=====================================================================*

; Local subroutine to save a few bytes on the repetitive operation of
; doing DosSemRequest(CtrlSem,-1).

    ReqCtrl proc near
                push    ds
                push    offset CtrlSem  ; push semaphore far *
                mov     ax, -1
                push    ax              ; push timeout interval
                push    ax              ; ..value of -1L = "forever"
                call    DosSemRequest
                ret
    ReqCtrl endp

; Local subroutine to save a few bytes on doing DosSemWait([ax], -1).
    WaitSem proc near
                push    ds
                push    ax              ; AX = offset of a semaphore
                mov     ax, -1
                push    ax
                push    ax              ; push -1L = "forever"
                call    DosSemWait
                ret
    WaitSem endp

 page
;*=====================================================================*
;*                                                                     *
;*  Readers: -- the logic of any reader procedure                      *
;*                                                                     *
;*      DosSemWait(RdrGate, -1)     -- defer to any writers            *
;*      DosSemRequest(CtrlSem, -1)  -- get exclusive use               *
;*      RdrCount = RdrCount + 1     -- check in as a reader            *
;*      DosSemClear(CtrlSem)        -- release modify-right            *
;*      -- note reader status in per-client data for termination       *
;*                                                                     *
;*      -- Here RdrCount is nonzero, so no Writer will be              *
;*      -- active and the reader may do its work.                      *
;*                                                                     *
;*      -- clear reader status in per-client data                      *
;*      DosSemRequest(CtrlSem, -1)                                     *
;*      RdrCount = RdrCount - 1     -- check out as a reader           *
;*      if RdrCount == 0 then       -- if last to leave,               *
;*          DosSemClear(WtrWait)      -- wake up any writers           *
;*      DosSemClear(CtrlSem)                                           *
;*                                                                     *
;*  end Readers.                                                       *
;*                                                                     *
;*=====================================================================*

    RdrIn   proc near
                push    ax
                push    dx
;       DosSemWait(RdrGate,-1)
                mov     ax, offset RdrGate
                call    WaitSem
;       DosSemRequest(CtrlSem, -1)
                call    ReqCtrl
;       RdrCount += 1
                inc     word ptr RdrCount
;       DosSemClear(CtrlSem)
                push    ds
                push    offset CtrlSem
                call    DosSemClear
; Since multiple readers are allowed, there could be multiple
; reader threads from one one process -- very unlikely but we
; have to permit it.  Therefore the per-client data has to be
; updated in a critical section, which freezes out other threads
; of this process but not threads of other processes...
                call    DosEnterCritSec
                call    GetPerClient
                inc     dh      ; count one more reader
                or      al, al  ; do we have an Exit routine?
                jnz     RIHasExit ; (yes)
                call    SetExitList ; no, set one up
                dec     al      ; ..and indicate so
RIHasExit:      call    PutPerCLient
                call    DosExitCritSec
                pop     dx
                pop     ax
                ret
    RdrIn   endp

    RdrOut  proc near
                push    ax      ; save caller's exit code
                push    dx
; Update per-client info for exit routine
                call    DosEnterCritSec
                call    GetPerClient
                dec     dh
                jns     ROPCOk
                xor     dh, dh  ; should never be used
ROPCOk:         call    PutPerClient
                call    DosExitCritSec

;       DosSemRequest(CtrlSem, -1)
                call    ReqCtrl
;       RdrCount -= 1
                dec     word ptr RdrCount
                js      RdrCntBad ; certain rare disasters
;       if RdrCount == 0
NotMinusRdr:    jnz     NotLastRdr
;           DosSemClear(WtrWait)
LastRdrOut:     push    ds
                push    offset WtrWait
                call    DosSemClear
;       DosSemClear(CtrlSem)
NotLastRdr:     push    ds
                push    offset CtrlSem
                call    DosSemClear
                pop     dx
                pop     ax
                ret
; should never occur: RdrCount was zero or minus when we dec'd it.
; set it to zero and pretend it never happened.  The alternative
; would be to leave it negative, which would freeze writers forever.
; Which is worse, uncontrolled writers or frozen ones?
RdrCntBad:      mov     RdrCount, 0
                jmp short LastRdrOut
    RdrOut  endp
page
;*=====================================================================*
;*                                                                     *
;*  Writers: -- the logic of any writer procedure                      *
;*                                                                     *
;*      DosSemRequest(CtrlSem, -1)  -- get exclusive use               *
;*      DosSemSet(RdrGate)          -- close gate to more readers      *
;*      WtrCount = WtrCount + 1     -- check in as a writer            *
;*      While RdrCount > 0 do       -- while any working readers,      *
;*          DosSemSet(WtrWait)         -- set sem while holding CtrlSem*
;*          DosSemClear(CtrlSem)       -- enable Readers to exit       *
;*          DosSemWait(WtrWait, -1)    -- wait for last Reader         *
;*          DosSemRequest(CtrlSem, -1) -- regain exclusion             *
;*      end while                                                      *
;*      -- note writer status in per-client data for termination       *
;*                                                                     *
;*      -- Here the writer owns CtrlSem and RdrCount is zero, so has   *
;*      -- exclusive use of the resource and may do its work.          *
;*                                                                     *
;*      -- clear writer status in per-client data                      *
;*      WtrCount = WtrCount - 1     -- check out as a writer           *
;*      if WtrCount == 0 then       -- if no other writers waiting,    *
;*          DosSemClear(RdrGate)      -- waken any waiting readers     *
;*      DosSemClear(CtrlSem)        -- let readers, other writers in   *
;*                                                                     *
;*  end Writers.                                                       *
;*                                                                     *
;*=====================================================================*

    WtrIn  proc near
                push    ax
                push    dx
                call    ReqCtrl
                push    ds
                push    offset RdrGate
                call    DosSemSet
                inc     word ptr WtrCount
WhileRdrs:      test    word ptr RdrCount, -1
                jz      NoRdrsNow
                push    ds
                push    offset WtrWait
                call    DosSemSet
                push    ds
                push    offset CtrlSem
                call    DosSemClear
                mov     ax, offset WtrWait
                call    WaitSem
                call    ReqCtrl
                jmp short WhileRdrs
NoRdrsNow:
; Since there's only one writer thread anywhere, we have exclusive
; use of this per-client data as well.
                call    GetPerClient
                or      al, al ; do we have an exit routine for you?
                jnz     WIHasExit ; (yes)
                dec     al      ; no, be we will
                call    SetExitList
WIHasExit:      mov     dl, 1
                call    PutPerClient
                pop     dx
                pop     ax
                ret
    WtrIn  endp

    WtrOut proc near
                push    ax
                push    dx
                call    GetPerClient
                mov     dl, 0
                call    PutPerClient
                dec     word ptr WtrCount
                js      BadWtrCount
                jnz     MoreWriters
LastWtrOut:     push    ds
                push    offset RdrGate
                call    DosSemClear
MoreWriters:    push    ds
                push    offset CtrlSem
                call    DosSemClear
                pop     dx
                pop     ax
                ret
; Should not occur: WtrCount was zero/minus when we dec'd it.
; Set it to zero and pretend it never happened.  The alternative
; is to leave it negative and freeze readers out forever.
BadWtrCount:    mov     WtrCount, 0
                jmp short LastWtrOut
    WtrOut endp

 subttl Heap Space Management
  page
;*=====================================================================*
;*                                                                     *
;* This is heap-object management.  At this level we are concerned     *
;* with managing a variable number of objects of variable size; we     *
;* don't care what they contain (both names and values are objects).   *
;*                                                                     *
;* Objects always are multiples of a word in size, word-aligned. All   *
;* objects begin with two words: a length word and a Control word.     *
;* The length contains the total length of the object, which is always *
;* even and at least 6.                                                *
;*                                                                     *
;* The Control word is the offset in the Array of the object's Anchor. *
;* That is, the word at SegLimit-Control contains the offset of this   *
;* object's contents (4 bytes into it).  Except inside these routines, *
;* EVERY reference to an object starts with its Anchor.  That gives us *
;* the freedom to move objects around during garbage collections, so   *
;* long as we update their anchors appropriately.                      *
;*                                                                     *
;* If an object's control word is zero, it is a free object, which is  *
;* to say it is garbage waiting to be collected.                       *
;*                                                                     *
;* The procedures in this group are:                                   *
;*      GColl : collect garbage and compress the heap.                 *
;*      XTend : extend the segment size and move the Array up.         *
;*      GetSpace(S) : allocate an object of size S, using a variety    *
;*                      of strategies for calling GColl or XTend       *
;*      MakeObj(C,S) : create an object of size S anchored at C        *
;*      FreeObj(C) : free the object whose anchor is C                 *
;*                                                                     *
;*=====================================================================*
        page
;*=====================================================================*
;*                                                                     *
;* This is the garbage-collect subroutine.  It is called from either   *
;* GetSpace or GetDesc, and this is its logic:                         *
;*                                                                     *
;*       P = HeapBot                                                   *
;*       while (P->C) do P += P->L                                     *
;*       /* P points to first garbage */                               *
;*       Q = P                                                         *
;*       while (Q < HeapMid) && (Q->C == 0) do Q += Q->L               *
;*       /* Q points to first data after garbage or to top of heap */  *
;*       while (Q < HeapMid)                                           *
;*               if (Q->C)                                             *
;*                       Anchors[C] = P+4                              *
;*                       Copy object *Q to *P for length Q->L          *
;*                       P += Q->L                                     *
;*               Q += Q->L                                             *
;*       HeapMid = P                                                   *
;*=====================================================================*

    GColl proc near
                push    bx
                push    cx
                push    si
                push    di
                push    es

                push    ds      ; get es==ds
                pop     es
                cld             ; and forward moves

;*       P = HeapBot                                                   *
                mov     di, word ptr HeapBot

;*       while (P->C) do P += P->L                                     *
LookForGarb:    test    word ptr [di + 2], -1
                jz      AtFirstGarb
                add     di, word ptr [di]
                jmp short LookForGarb
AtFirstGarb:

;*       /* P points to first garbage */                               *
;*       Q = P                                                         *
                mov     si, di

;*       while (Q < HeapMid) && (Q->C == 0) do Q += Q->L               *
LookForObj:     cmp     si, word ptr HeapMid
                jnb     AtNextObj
                test    word ptr [si + 2], -1
                jnz     AtNextObj
                add     si, word ptr [si]
                jmp short LookForObj
AtNextObj:

;*       /* Q points to first data after garbage or to top of heap */  *
;*       while (Q < HeapMid)                                           *
Collect:        cmp     si, word ptr HeapMid
                jnb     BeyondGarb

;*               if (Q->C)                                             *
                test    word ptr [si + 2], -1
                jz      SkipGarb

;*                       Anchors[C] = P+4
                mov     bx, word ptr SegLimit
                sub     bx, word ptr [si + 2] ; bx->Anchor[C]
                lea     cx, word ptr [di + 4]
                mov     word ptr [bx], cx ; Anchor[C] = P+4

;*                       Copy object *Q to *P for length Q->L          *
;*                       P += Q->L                                     *
                mov     cx, word ptr [si] ; cx=length, always even
                shr     cx, 1           ; move by words for speed
             rep movsw
                jmp short Collect       ; "Q" updated by move

;*               Q += Q->L                                             *
SkipGarb:       add     si, word ptr [si]
                jmp short Collect

;*       HeapMid = P                                                   *
BeyondGarb:     mov     word ptr HeapMid, di

                pop     es
                pop     di
                pop     si
                pop     cx
                pop     bx
                ret
    GColl endp
        page
;*=====================================================================*
;*                                                                     *
;* This is the segment-extend routine, XTend(ax=amount).  It is called *
;* either from GetSpace or GetDesc to make GLOBDATA space by extending *
;* it to higher addresses and moving the Array up in it.  If adding    *
;* ax=amount to the present segment size would blow the 64K limit,     *
;* we return with carry set.  Otherwise we use DosReallocSeg to        *
;* make the segment bigger.  If that fails we return with carry set.   *
;*                                                                     *
;* When the segment did stretch, we relocate the array of anchor words *
;* to its new high end, adjust HeapTop, and return carry clear.        *
;*                                                                     *
;*=====================================================================*


    Xtend proc near
                push    bx
                push    cx
                push    si
                push    di
                push    es

                add     ax, word ptr SegLimit
                jc      Over64K         ; (blows the limit)

                mov     di, ax          ; di = future SegLimit
                push    ax
                push    ds
                call    DosReallocSeg   ; (size, selector)
                or      ax, ax
                stc
                jnz     ReallocFailed

                mov     si, word ptr SegLimit
                sub     si, 2           ; si -> last word in array
                mov     word ptr SegLimit, di
                sub     di, 2           ; di -> last word in segment

                mov     cx, ArraySize   ; cx = # of 4-word groups
                shl     cx, 2           ; cx = # of words
                push    ds              ; ds:si->last word to move
                pop     es              ; es:di->where to move it
                std                     ; move runs downwards
             rep movsw
                add     di, 2           ; di -> last-moved word
                mov     word ptr HeapTop, di
                clc
Over64K:
ReallocFailed:
                pop     es
                pop     di
                pop     si
                pop     cx
                pop     bx
                ret
    Xtend endp
        page
;*=====================================================================*
;*                                                                     *
;* This is GetSpace(cx=S).  It is called only from MakeObj().          *
;* Its job is to find space for an object with S bytes of contents,    *
;* returning the offset or zero for failure.                           *
;*                                                                     *
;* The first step it is to increment S by 4 to allow for Length and    *
;* Control words, and to round it up to a word multiple.               *
;*                                                                     *
;* Then we start on our strategies.  They are chosen based on the      *
;* assumptions that GColl is a costly routine to call, so we want to   *
;* call it as rarely as possible.  But Xtend is just as costly and     *
;* could involve us in segment swapping delays, so we want to call it  *
;* even less often.                                                    *
;*                                                                     *
;* 1. If there's room between HeapMid and HeapTop, use it.  That's     *
;*    easy and quick both.                                             *
;*                                                                     *
;* 2. Ok, if HeapGarb is at least 8*S, do a garbage collection and     *
;*    then allocate space at HeapMid.  The 8* multiplier is to reduce  *
;*    the number of GColls when a couple of variables are being        *
;*    reassigned over and over.  The next 7 calls can likely use (1).  *
;*                                                                     *
;* 3. No good?  We must be low on space.  Let Z be S rounded up to a   *
;*    multiple of 2048.   If we can Xtend by that much, do it.  The    *
;*    extra bytes will keep us up in step (1) for the next few calls.  *
;*                                                                     *
;* 4. If that fails we can get nervous.  If HeapGarb is at least S,    *
;*    call GColl and use the space it frees.                           *
;*                                                                     *
;* 5. No YET?  Ok, if we can Xtend by exactly S, do that.              *
;*                                                                     *
;* If all the above fail, return an offset of zero to say failure.     *
;*                                                                     *
;*=====================================================================*

    GetSpace proc near

                push    bx
                push    cx

                mov     ax, cx
                add     ax, 4+1 ; extend S for Length, Control
                and     ax, -2  ; ..and round to words.
                mov     cx, ax  ; save in CX for later.

Strategy1:      add     ax, HeapMid
                cmp     ax, HeapTop
                ja      Strategy2
                jmp     ThereIsRoom

Strategy2:      mov     ax, HeapGarb
                shr     ax, 3
                cmp     ax, cx
                jb      Strategy3
                call    GColl
                jmp     ThereIsRoom

Strategy3:      mov     ax, cx
                add     ax, 2047
                and     ax, -2048
                call    Xtend
                jc      Strategy4
                jmp     ThereIsRoom

Strategy4:      cmp     cx, HeapGarb
                ja      Strategy5
                call    GColl
                jmp     ThereIsRoom

Strategy5:      mov     ax, cx
                call    Xtend
                jnc     ThereIsRoom
                mov     ax, 0
                jmp     GSexit

; We get here when we know that HeapMid+S <= HeapTop, and therefore...
ThereIsRoom:
                mov     bx, HeapMid
                mov     word ptr [bx], cx       ; set the L word
                mov     word ptr [bx+2], 0      ; set zero control
                add     cx, bx
                mov     HeapMid, cx
                lea     ax, [bx + 4]

; We get here with the desired offset, or zero, in AX
GSexit:         pop     cx
                pop     bx
                ret
    GetSpace endp
 page
;*=====================================================================*
;*                                                                     *
;* This is FreeObj(dx=Control), to free the object owned by anchor     *
;* word Array[Control] if there is one.                                *
;*                                                                     *
;*=====================================================================*

    FreeObj proc near
                push    ax
                push    bx
                push    di

                mov     bx, SegLimit
                sub     bx, dx
                mov     di, word ptr [bx]
                or      di, di
                jz      NoObjectToFree
                mov     ax, word ptr [di - 4]   ; get object's size
                add     ax, HeapGarb
                mov     HeapGarb, ax            ; update garbage count
                mov     word ptr [di - 2], 0    ; mark object garbage
                mov     word ptr [bx], 0        ; unhook anchor

NoObjectToFree:
                pop     di
                pop     bx
                pop     ax
                ret
    FreeObj endp
 page
;*=====================================================================*
;*                                                                     *
;* This is MakeObj(cx=Size, dx=Control), to create an object of Size   *
;* under anchor word Array[Control], returning ax=0 if we succeed and  *
;* ax = 3 if we can't get room.                                        *
;*                                                                     *
;* Incidentally if that Anchor presently has an object, we free it.    *
;*                                                                     *
;*=====================================================================*
    MakeObj proc near
                push    bx
                push    di

                call    FreeObj         ; free present value
                call    GetSpace        ; get new space?
                or      ax, ax
                jnz     GotTheSpace
                mov     ax, Err_no_room
                jmp     MOExit

GotTheSpace:    mov     di, ax
                mov     word ptr [di - 2], dx ; set control wd
                mov     bx, SegLimit
                sub     bx, dx          ; bx->anchor
                mov     word ptr [bx], di
                xor     ax, ax          ; return 0 for success

MOExit:         pop     di
                pop     bx
                ret
    MakeObj endp
 subttl Descriptor Array Management
 page
;*=====================================================================*
;*                                                                     *
;*  Pay attention, this is highly sophisticated.  Really.  Now, what   *
;* we gotta do is create the logical effect of four parallel arrays:   *
;*                                                                     *
;*   indirect      For each defined      names    values   sizes       *
;*  +--------+     name there is a     +--------+--------+--------+    *
;*  |        |     dedicated row of    |        |        |        |    *
;*  +--------+     three adjacent      +--------+--------+--------+    *
;*  |        |     words: anchors for  |        |        |        |    *
;*  +--------+     the name object and +--------+--------+--------+    *
;*  |        |     the value object if |        |        |        |    *
;*                 any, and the size                                   *
;* of the value. These descriptors are allocated first come first      *
;* served and when a name is deleted the descriptor is zeroed, and     *
;* reused for later names as needed.                                   *
;*                                                                     *
;* However we need to keep track of names in lexical order.  For this  *
;* purpose we have a separate array in which we store the indexes of   *
;* the descriptors in their sequence.  When a name is deleted, the     *
;* indirect array has to be compressed, so its vacant entries are      *
;* always at the bottom.                                               *
;*                                                                     *
;* OK, now we can't allocate a fixed size of these since we haven't    *
;* got a clue as to how many names will be defined in a typical        *
;* system, or even if there is such a thing as "typical."  So these    *
;* arrays have to expand as names are defined.  Great; the heap grows  *
;* up from low addresses, we will have the array grow down from high   *
;* ones.  Wait a minute, there's TWO arrays, and BOTH grow...          *
;*                                                                     *
;* Solution: the arrays grow at the same rate, so interleave them.     *
;* Every added name adds four words down from the end of the segment,  *
;* three of which are its descriptor and the fourth is available for   *
;* extending the Indirect array.  The whole array is always at the     *
;* very end of the segment, so it can always be addressed by offsets   *
;* backwards from SegLimit.  The heap can be grown by extending the    *
;* segment and then sliding the array out to the end (see Xtend()).    *
;*                                                                     *
;* To index the Indirect array, jump by 8 bytes.  That is, Indirect[j] *
;* is the word *(Seglimit - 8j - 8).  To index the descriptors,        *
;* also jump by 8 but start higher.  To find Descrip[k,name] go to     *
;* (Seglimit - 8k - 6), and so forth.                                  *
;*                                                                     *
;* Actually we don't go through the whole charade of keeping index     *
;* numbers j or k; we store the offset of words from the end of the    *
;* segment -- not j or k but (8j+8) or (8k+6).  Thus the control word  *
;* in the name object of name k is (8k+6), and in the value object of  *
;* name k is (8k+4).  And the indirect entry that points to name k     *
;* also contains (8k+6).                                               *
;*                                                                     *
;* I think we'll call these backward offsets, backoffs.                *
;*                                                                     *
;* Notice that we return backoffs, not offsets.  Also note that our    *
;* callers will carry these as backoffs.  The reason is that if you    *
;* call MakeObj(), the segment could get extended, which would make    *
;* an offset invalid -- but backoffs aren't affected.                  *
;*                                                                     *
;* GetDesc() : allocate a descriptor trio and return dx=backoff of the *
;* first word, the name-anchor.                                        *
;*                                                                     *
;* GetIWord() : locate the first free Indirect word and return its     *
;* backoff in dx.  There is an assumption that GetDesc will always     *
;* be called before GetIWord.                                          *
;*                                                                     *
;* GetDesc may have to extend the Arrays down into the heap to get     *
;* room.  If the heap's full, it tries first to GColl, then to Xtend   *
;* by 1K.  If it can't get room at all it returns carry set.           *
;*                                                                     *
;*=====================================================================*

    GetDesc proc near

                push    ax
                push    bx
                test    word ptr ArrayFree, -1
                jz      ArrayIsFull

; There's an empty descriptor somewhere in the array.  Scan for it.
                mov     bx, SegLimit
                sub     bx, 6
GDScanDesc:     test    word ptr [bx], -1
                jz      GotMTDesc
                sub     bx, 8
                jmp short GDScanDesc

; The array is full; create an empty entry by extending it backward.
ArrayIsFull:    mov     bx, HeapMid
                add     bx, 8
                cmp     bx, HeapTop
                jbe     RoomToGrow

; There isn't room to extend down.  If there's garbage, collect it.
                test    word ptr HeapGarb, 0fff0h ; at least 16 bytes?
                jz      TryStretch
                call    GColl
                jmp short RoomToGrow ; (there is now)

; There's no garbage either, try to extend, and if that fails, quit.
TryStretch:     mov     ax, 1024
                call    Xtend
                jnc     RoomToGrow
                jmp short GDExit ; with carry set

; One way or another there is room to extend the Array space 4 words.
; Do that, zero the contents, and call it a free entry.
RoomToGrow:     mov     bx, HeapTop
                sub     bx, 8
                mov     HeapTop, bx
                add     bx, 2           ; bx->name word
                mov     word ptr [bx], 0
                mov     word ptr [bx+2], 0
                mov     word ptr [bx+4], 0
                inc     ArraySize
                inc     ArrayFree

; Here, bx->an empty descriptor.  Convert that positive offset to
; a backoff in dx.  Decrement ArrayFree since this desc is in use.
GotMTDesc:      mov     dx, SegLimit
                sub     dx, bx
                dec     ArrayFree
                clc

; Exit here with carry set or not, as appropriate
GDExit:         pop     bx
                pop     ax
                ret
    GetDesc endp

;*=====================================================================*
;*                                                                     *
;* This is GetIWord(). On the assumption that GDesc() has been called  *
;* first, we can be sure that one more descriptor has been created     *
;* than has been sorted, and therefore there is an Array word free for *
;* the Indirect array entry.                                           *
;*   Since the Indirect list is kept compact under deletions, its      *
;* first free entry is the word at (HeapTop + 8*(ArrayFree)).          *
;*                                                                     *
;*=====================================================================*
    GetIWord proc near
                mov     dx, ArrayFree
                shl     dx, 3
                add     dx, HeapTop
                sub     dx, SegLimit
                neg     dx              ; make a backoff of it.
                ret
    GetIWord endp

 subttl String Operations
 page
;*=====================================================================*
;*                                                                     *
;*  These simple string operations encapsulate the CPU's string ops    *
;* and provide for preserving the DI and SI regs for reuse.            *
;*                                                                     *
;* StrLen( es:di ) : cx=length inclusive of null at end                *
;*                                                                     *
;* StrCpy( ds:si to es:di for cx )   using word copies for speed       *
;*                                                                     *
;* StrCmp( ds:si vs es:di to a null) : sign and Z flag                 *
;*                                                                     *
;*=====================================================================*

    StrLen proc near

                push    di
                push    ax
                mov     cx, -1          ; asciiz strings go forever
                xor     ax, ax          ; up to a null, anyway
                cld
        repne   scasb                   ; decrements cx at least once
;                          cx = -(nonzero length + 2)
                inc     cx
                neg     cx
                pop     ax
                pop     di
                ret
    StrLen endp

    StrCpy proc near

                push    si
                push    di
                push    cx
                cld
                test    cx, 1
                jz      CXIsEven
                movsb                   ; move the odd byte
CXIsEven:       shr     cx, 1           ; get count of words
                jcxz    CXIsZero        ; allow for 1 and zero
             rep movsw                  ; move words

CXIsZero:       pop     cx
                pop     di
                pop     si
                ret
    StrCpy endp

    StrCmp proc near

                push    ax
                push    di
                push    si
                mov     al, byte ptr [si]
                cmp     al, 1 ; is left string a null?
                jb      StrMisMatch ; (if so, it's "low")

StrStillEqual:  lodsb           ; al = *(ds:si++)
                scasb           ; flags = (as :: *(es:di++))
                jnz     StrMisMatch ; stop at first mismatch
                or      al, al  ; equal nulls?
                jnz     StrStillEqual ; equal nonnulls, continue
StrMisMatch : ; or end of equal strings

                pop     si
                pop     di
                pop     ax
                ret
    StrCmp endp
 subttl Name-Search Routines
page
;*=====================================================================*
;*                                                                     *
;* SearchGE(es:di) compares the string es:di to each of the defined    *
;* names in ascending lexical order until (a) the end of the array     *
;* is found, (b) a match is found, (c) a lexically-greater name is     *
;* seen.  In case (b) the Z flag will be true and dx will contain the  *
;* backoff of the matching name's descriptor words.  In case (c) the   *
;* flags will be set for "ja" (es:di is less, defined name is "above") *
;* and in case (a) the flags will be set for "jb" (es:di is greater    *
;* and last-tested name was "below," or else the address of the next   *
;* Indirect word was "below" the last one of all).                     *
;*                                                                     *
;*=====================================================================*
    SearchGE proc near
                push    ax
                push    bx
                push    cx
                push    si
                mov     cx, ArrayFree
                shl     cx, 3
                add     cx, HeapTop     ; cx->last valid Indirect Wrd
                mov     bx, SegLimit

SGELoop:        sub     bx, 8           ; bx->next Indirect word
                cmp     bx, cx
                jb      SGEExit
                mov     dx, word ptr [bx] ; dx is descriptor backoff
                mov     si, SegLimit
                sub     si, dx          ; si->descriptor
                mov     si, word ptr [si] ; si->name object
                call    StrCmp
                jb      SGELoop
; search is over, dx = backoff of desc of name >= es:di
; search is over, or failed
SGEExit:        pop     si
                pop     cx
                pop     bx
                pop     ax
                ret
    SearchGE endp

;*=====================================================================*
;*                                                                     *
;* SearchGT(es:di) does the same as SearchGE() but rejects the equal   *
;* condition.  The code differs only in the jump condition at the end  *
;* of the search loop.                                                 *
;*                                                                     *
;*=====================================================================*
    SearchGT proc near
                push    ax
                push    bx
                push    cx
                push    si
                mov     cx, ArrayFree
                shl     cx, 3
                add     cx, HeapTop     ; cx->last valid Indirect Wrd
                mov     bx, SegLimit

SGTLoop:        sub     bx, 8           ; bx->next Indirect word
                cmp     bx, cx
                jb      SGTExit
                mov     dx, word ptr [bx] ; dx is descriptor backoff
                mov     si, SegLimit
                sub     si, dx          ; si->descriptor
                mov     si, word ptr [si] ; si->name object
                call    StrCmp
                jbe     SGTLoop
; search is over, dx = backoff of desc of name > es:di
; search is over, or failed
SGTExit:        pop     si
                pop     cx
                pop     bx
                pop     ax
                ret
    SearchGT endp

 subttl The ENTER Funtion
 page
;*=====================================================================*
;*                                                                     *
;* GLBENTER(in: name).  This is a Writer procedure.                    *
;*                                                                     *
;*      Look up the name; if an equal condition is found return error  *
;* name-exists.  Get a descriptor; if fails, return error no-room.     *
;* Get an object for the name; if fails return no-room.  Copy name     *
;* string into object.  Get Indirect word.  Insertion-sort the new     *
;* new name into the Indirect array.                                   *
;*                                                                     *
;*=====================================================================*

EntParmLen      equ     4       ; one far ptr is only parm
EntNameAdr      equ     6       ; bp offset of first parm
EntSave1stGT    equ     -2      ; scratch word

    GLBENTER proc far

                push    bp
                mov     bp, sp
                sub     sp, 2   ; make room on stack

                push    bx
                push    cx
                push    dx
                push    si
                push    di

                push    ds
                push    es
                mov     ax, seg GLOBDATA
                mov     ds, ax

; Get exclusive access to GLOBDATA
                call    WtrIn

; look up name; if a match is found return error_name_exists
                les     di, dword ptr [bp + EntNameAdr]
                call    SearchGE
                jne     EntSaveInsPt
                mov     ax, Err_name_exists
                jmp     EntExit

; save insertion point: if there is a lexically greater name, dx
; contains the backoff of its descriptor.  Save 0 if there is no
; greater name.
EntSaveInsPt:   ja      EntSaveIns2
                mov     dx, 0   ; indicate "none greater"
EntSaveIns2:    mov     [bp + EntSave1stGT], dx

; get a descriptor and handle a failure
EntGetDesc:     call    GetDesc
                jnc     EntGetObj
                mov     ax, Err_no_room
                jmp     EntExit

; get an object to hold the name (dx now has backoff of descriptor)
EntGetObj:      call    StrLen          ; cx = length of *es:di
                call    MakeObj         ; (cx, dx)
                or      ax, ax          ; got it?
                jz      EntCopy         ; (yes)
                jmp     EntExit         ; (no, ax=error code)

; copy name string into object.
EntCopy:        mov     bx, SegLimit
                sub     bx, dx
                mov     di, word ptr [bx]
                push    ds
                pop     es              ; es:di -> object
                lds     si, dword ptr [bp + EntNameAdr]
                call    StrCpy
                push    es              ; es:di -> object with name
                pop     ds              ; restore ds base

; Get the Indirect word at the end of the array and put our
; descriptor's backoff into it.  Then swap it into position in the
; array.  The right position is the one now occupied by the first name
; that is lexically greater, and we saved its descriptor backoff earlier.
                mov     cx, dx          ; save our descriptor backoff
                call    GetIWord        ; dx = IWord at end, that of
                mov     bx, SegLimit    ; ..a name greater than all
                sub     bx, dx          ; bx->end Iword
; We saved a zero earlier if there was no greater name.  If there is
; no greater name, the right place for our descriptor is the end spot.
                test    word ptr [bp + EntSave1stGT], -1 ;
                jz      EntStoreIt      ; no, put it here
; Slide all greater names down in the the Indirect list and put our
; new name in the spot now occupied by the next-greater one.  The
; backoff of that name is saved on the stack.
EntSwapIword:   mov     ax, word ptr [bx+8] ; get next Iword
                mov     word ptr [bx], ax ; and pull it down
                add     bx, 8           ; step to next
                cmp     ax, word ptr [bp + EntSave1stGT] ; this one?
                jne     EntSwapIword    ; no, keep swapping
EntStoreIt:     mov     word ptr [bx], cx ; store desc in place
                mov     ax, 0           ; set zero retcode

; Exit after setting ax = result code
EntExit:        call    WtrOut
                pop     es
                pop     ds
                pop     di
                pop     si
                pop     dx
                pop     cx
                pop     bx
                pop     bp      ; our scratch word
                pop     bp      ; the saved bp
                ret     EntParmLen
    GLBENTER endp
 subttl The STORE Funtion
 page
;*=====================================================================*
;*                                                                     *
;* GLBSTORE(in: name, value, length).  This is a writer procedure      *
;*                                                                     *
;*      Look up the name; if an equal condition is not found return    *
;* error no-name.  Using the length given, make a new object           *
;* anchored in the value word for this name.  Copy the value to the    *
;* new object.  Set the length word.                                   *
;*                                                                     *
;*=====================================================================*

StoParmLen equ 10       ; ten bytes of parm
StoNameAdr equ 12       ; bp offset of name
StoValAdr  equ  8       ; bp offset of value
StoLenAdr  equ  6       ; bp offset of value

    GLBSTORE proc far

                push    bp
                mov     bp, sp

                push    bx
                push    cx
                push    dx
                push    si
                push    di

                push    ds
                push    es
                mov     ax, seg GLOBDATA
                mov     ds, ax

; Get exclusive access to GLOBDATA
                call    WtrIn

; look up name; if no match is found return err_no_name
                les     di, dword ptr [bp + StoNameAdr]
                call    SearchGE
                je      StoMakeIt
                mov     ax, err_no_name
                jmp     StoExit

; Make a new object for the value of the found name
StoMakeIt:      sub     dx, 2   ; make the backoff of the value
                call    FreeObj ; get rid of present value
                mov     cx, word ptr [bp + StoLenAdr]
                jcxz    StoCopyIt
                call    MakeObj
                or      ax, ax  ; got it?
                jz      StoCopyIt
                jmp     StoExit ; (no, and ax=error code)

; Copy the value to the new object, if there is a value.
StoCopyIt:      push    ds
                pop     es
                mov     bx, SegLimit
                sub     bx, dx
                mov     word ptr [bx + 2], cx ; set length word
                jcxz    StoNoCopy ; don't load meaningless address
                mov     di, [bx]
                lds     si, [bp + StoValAdr]
                call    StrCpy
                push    es      ; recover our DS
                pop     ds
StoNoCopy:      xor     ax, ax

; Exit with ax set to appropriate error code
StoExit:        call    WtrOut
                pop     es
                pop     ds
                pop     di
                pop     si
                pop     dx
                pop     cx
                pop     bx
                pop     bp
                ret     StoParmLen
    GLBSTORE endp
 subttl The DELETE Funtion
 page
;*=====================================================================*
;*                                                                     *
;* GLBDELETE(in: name).  This is a writer procedure.                   *
;*                                                                     *
;*      Look up the name; if an equal condition is not found return    *
;* error no-name.  Use FreeObj to free the name and value objects      *
;* (which sets zero in the name and value words) and set zero in the   *
;* length word.  Increment the count of free names.  Run down the      *
;* Indirect list and find the word that points to the deleted name,    *
;* then continue down the list compacting it up.                       *
;*                                                                     *
;*=====================================================================*

DelNameAdr equ  6       ; bp offset of value
DelParmLen equ  4       ; four bytes of parameters

    GLBDELETE proc far

                push    bp
                mov     bp, sp

                push    bx
                push    cx
                push    dx
                push    si
                push    di

                push    ds
                push    es
                mov     ax, seg GLOBDATA
                mov     ds, ax

; Get exclusive access to GLOBDATA
                call    WtrIn

; look up name; if no match is found return err_no_name
                les     di, dword ptr [bp + DelNameAdr]
                call    SearchGE
                je      DelKillIt
                mov     ax, err_no_name
                jmp     DelExit

; save a copy of the name's backoff for use in compacting
DelKillIt:      mov     cx, dx

; Free the name and value objects, zero the length
                call    FreeObj         ; free name object
                sub     dx, 2           ; make backoff of value
                call    FreeObj         ; free value if any
                add     dx, 2
                mov     bx, SegLimit
                sub     bx, dx
                mov     word ptr [bx + 4], 0

; Scan down the Indirect array looking for the one that pointed
; to this name.  There has to be one.

                mov     bx, SegLimit
DelScan1:       sub     bx, 8
                cmp     cx, [bx]
                jnz     DelScan1

; Scan the rest of the way back bringing name entries up over this
; one.  Don't scan past the current end.
                mov     cx, ArrayFree   ; free before this one
                shl     cx, 3
                add     cx, HeapTop ; cx -> present last entry

DelScan2:       cmp     bx, cx
                jz      DelScanOver
                mov     ax, word ptr [bx - 8]
                mov     word ptr [bx], ax
                sub     bx, 8
                jmp short DelScan2

DelScanOver:    xor     ax, ax
                inc     ArrayFree

; Exit with ax set to appropriate error code
DelExit:        call    WtrOut
                pop     es
                pop     ds
                pop     di
                pop     si
                pop     dx
                pop     cx
                pop     bx
                pop     bp
                ret     DelParmLen
    GLBDELETE endp

 subttl The QUERY Function
 page
;*=====================================================================*
;*                                                                     *
;* GLBQUERY(in: name; out: sizeword) : this is a Reader procedure      *
;*                                                                     *
;*      Look up the name; if an equal condition is not found return    *
;* error no-name.  Pick up the name's size-word and store it in the    *
;* second parameter, and return zero.                                  *
;*                                                                     *
;*=====================================================================*

QryNameAdr equ  10      ; bp offset of name pointer
QrySizeAdr equ  6       ; bp offset of size-word pointer
QryParmLen equ  8       ; bytes of parameters

    GLBQUERY proc far

                push    bp
                mov     bp, sp

                push    bx
                push    cx
                push    dx
                push    si
                push    di

                push    ds
                push    es
                mov     ax, seg GLOBDATA
                mov     ds, ax

; Get shared access to GLOBDATA
                call    RdrIn

; look up name; if no match is found return err_no_name
                les     di, dword ptr [bp + QryNameAdr]
                call    SearchGE
                je      QueryIt
                mov     ax, err_no_name
                jmp     QryExit

; dx is the backoff of the name's descriptor, get bx->descriptor
QueryIt:        mov     bx, SegLimit
                sub     bx, dx
                mov     ax, word ptr [bx + 4] ; ax = size

; store the length in the given word
                les     di, dword ptr [bp + QrySizeAdr]
                mov     word ptr es:[di], ax
                xor     ax, ax

; Exit with ax set to appropriate error code
QryExit:        call    RdrOut
                pop     es
                pop     ds
                pop     di
                pop     si
                pop     dx
                pop     cx
                pop     bx
                pop     bp
                ret     QryParmLen
    GLBQUERY endp

 subttl The FETCH Function
 page
;*=====================================================================*
;*                                                                     *
;* GLBFETCH(in: name; out: valbuff, sizeword) : a Reader procedure     *
;*                                                                     *
;*      Look up the name; if an equal condition is not found return    *
;* error no-name.  Select the lesser of the name's value size and the  *
;* passed size word.  Set that in the size word.  Use it as a copy     *
;* length in copying the value (if any) to the value buffer.  Return   *
;* either zero or Err-trunc-val.                                       *
;*                                                                     *
;*=====================================================================*

FetNameAdr equ  14      ; bp offset of name pointer
FetBuffAdr equ  10      ; bp offset of value buffer pointer
FetSizeAdr equ  6       ; bp offset of size-word pointer
FetParmLen equ  12      ; bytes of parameters

    GLBFETCH proc far

                push    bp
                mov     bp, sp

                push    bx
                push    cx
                push    dx
                push    si
                push    di

                push    ds
                push    es
                mov     ax, seg GLOBDATA
                mov     ds, ax

; Get shared access to GLOBDATA
                call    RdrIn

; look up name; if no match is found return err_no_name
                les     di, dword ptr [bp + FetNameAdr]
                call    SearchGE
                je      FetGetDesc
                mov     ax, err_no_name
                jmp     FetExit

; dx is the backoff of the name's descriptor, get bx->descriptor
FetGetDesc:     mov     bx, SegLimit
                sub     bx, dx

; address the passed size-word and check the sizes.
                les     di, dword ptr [bp + FetSizeAdr]
                mov     cx, word ptr [bx + 4] ; ax = actual size
                cmp     cx, word ptr es:[di] ; ..versus size limit
                jbe     FetUseValSize
                mov     cx, word ptr es:[di] ; truncate to limit
FetUseValSize:  mov     word ptr es:[di], cx ; set size returned

; copy the value to the supplied buffer -- provided the size is
; nonzero (don't load valbuff address when either the value is
; null or the passed size was zero).

                jcxz    FetNoCopy
                mov     si, word ptr [bx + 2] ; ds:si -> value object
                les     di, dword ptr [bp + FetBuffAdr]
                call    StrCpy

; set the return code depending on whether we truncated
FetNoCopy:      xor     ax, ax  ; assume zero return
                cmp     cx, word ptr [bx + 4]
                je      FetExit
                mov     ax, Err_trunc_val

; Exit with ax set to appropriate error code
FetExit:        call    RdrOut
                pop     es
                pop     ds
                pop     di
                pop     si
                pop     dx
                pop     cx
                pop     bx
                pop     bp
                ret     FetParmLen
    GLBFETCH endp

 subttl the NEXT function
 page
;*=====================================================================*
;*                                                                     *
;*  GLBNEXT(in: name, strmax; out: string, size) : a Reader proc       *
;*                                                                     *
;*  Locate the next name using SearchGT.  If there is none, return     *
;*  Err_no_name.  Otherwise get the name's length with StrLen. If it's *
;*  greater than strmax, return Err_no_room.  Otherwise set the size   *
;*  of the value in the size word and copy the name string to string.  *
;*                                                                     *
;*=====================================================================*

NxtNameAdr equ 16       ; bp offset of name far ptr
NxtStrMax  equ 14       ; bp offset of strmax word
NxtBuffAdr equ 10       ; bp offset of string far ptr
NxtSizeAdr equ 6        ; bp offset of size far ptr
NxtParmLen equ 14       ; bytes of parameters

    GLBNEXT proc far

                push    bp
                mov     bp, sp

                push    bx
                push    cx
                push    dx
                push    si
                push    di

                push    ds
                push    es
                mov     ax, seg GLOBDATA
                mov     ds, ax

; Get shared access to GLOBDATA
                call    RdrIn

; Look for a next name to the input name and quit if there is none.
                les     di, dword ptr [bp + NxtNameAdr]
                call    SearchGT
                ja      NxtGotOne
                mov     ax, Err_no_name
                jmp     NxtExit

; Get the length of the found name.
NxtGotOne:      mov     bx, SegLimit
                sub     bx, dx          ; bx -> descriptor
                push    ds
                pop     es
                mov     di, word ptr [bx] ; es:di -> name string
                call    StrLen          ; cx = length
                mov     si, di          ; save ds:si -> name string

; Check that the name will fit in the supplied buffer.
                cmp     cx, word ptr [bp + NxtStrMax]
                jbe     NxtNameFits
                mov     ax, Err_no_room
                jmp     NxtExit

; Set the name's value size in the last parameter
NxtNameFits:    les     di, dword ptr [bp + NxtSizeAdr]
                mov     ax, word ptr [bx + 4]
                mov     word ptr es:[di], ax

; Copy the name string to the output buffer
                les     di, dword ptr [bp + NxtBuffAdr]
                call    StrCpy
                xor     ax, ax

; Exit with ax set to appropriate error code
NxtExit:        call    RdrOut
                pop     es
                pop     ds
                pop     di
                pop     si
                pop     dx
                pop     cx
                pop     bx
                pop     bp
                ret     NxtParmLen
    GLBNEXT endp

 subttl Workspace Integrity Check
 page
;*=====================================================================*
;*                                                                     *
;* This routine is called to perform an integrity check of the common  *
;* segment.  It may be have been called for diagnostic purposes, but   *
;* in production it is called from ExitRoutine when a client process   *
;* is terminating while still an active writer.  This could mean that  *
;* we've had the bad luck to be struck by Control-break or a KillProc, *
;* but it might mean a bug in this code or garbage in GlobData.        *
;*   Oh, dear!  Is GlobDat still usable?  What we do here is attempt   *
;* to validate the many logical propositions built into the structure  *
;* of the heap and arrays.  If they all check out, other processes can *
;* continue.  If any one of them fails, we have to make the segment    *
;* usable again, and we do that by the brutal method of clearing it to *
;* its initial state.                                                  *
;*                                                                     *
;*=====================================================================*
    Integrity proc near
                push    ax
                push    bx
                push    cx
                push    dx
                push    di

; check #1: HeapBot even, and <= HeapMid
Ick1:           mov     ax, HeapBot
                test    ax, 1
                jz      Ick1a
                jmp     Ickfail
Ick1a:          cmp     ax, HeapMid
                jbe     Ick2
                jmp     IckFail

; check 2: HeapMid even, and <= HeapTop
Ick2:           mov     ax, HeapMid
                test    ax, 1
                jz      Ick2a
                jmp     IckFail
Ick2a:          cmp     ax, HeapTop
                jbe     Ick3
                jmp     IckFail

; check 3: HeapTop even, and <= SegLimit (which may be 0=65K)
Ick3:           mov     ax, HeapTop
                test    ax, 1
                jz      Ick3a
                jmp     IckFail
Ick3a:          cmp     ax, SegLimit
                jbe     Ick3b
                test    SegLimit, -1
                jz      Ick3b
                jmp     IckFail

; check 3b: HeapGarb even and <= HeapMid-EndFixed
Ick3b:          mov     ax, HeapGarb
                test    ax, 1
                jz      Ick3c
                jmp     IckFail
Ick3c:          add     ax, (offset GLOBDATA:EndFixed)
                cmp     ax, HeapMid
                jbe     Ick4
                jmp     IckFail

; check 4: ArrayFree <= ArraySize
Ick4:           mov     ax, ArraySize
                cmp     ax, ArrayFree
                jae     Ick5
                jmp     IckFail

; check 5: SegLimit - (ArraySize * 8) == HeapTop
Ick5:           shl     ax, 3
                neg     ax
                add     ax, SegLimit
                cmp     ax, HeapTop
                jz      Ick6
                jmp     IckFail

; Simpler sanity checks are ok, let's try something hard:
;       for( P = HeapBot, G=0; P += P->L; P < HeapMid )
;               check P->L is even, L>=6, and P+L <= HeapMid
;               if (P->C) then
;                       check P->C even and < SegLimit
;                       Q = SegLimit - P->C
;                       check Q > HeapTop
;                       check Q->word == P+4
;               else G += P->L
;       check G == HeapGarb
;
Ick6:           mov     cx, 0
                mov     di, HeapBot
IckObjLoop:     cmp     di, HeapMid
                jae     IckObjOver
                mov     ax, word ptr [di]
                test    ax, 1
                jnz     IckObjErr
                cmp     ax, 6
                jb      IckObjErr
                add     ax, di
                cmp     ax, HeapMid
                ja      IckObjErr
                mov     bx, word ptr [di + 2]
                cmp     bx, 0
                jnz     IckObjActive
                add     cx, word ptr [di]
                jmp short IckObjStep
IckObjActive:   test    bx, 1
                jnz     IckObjErr
                cmp     bx, SegLimit
                jb      IckObj2
                test    Seglimit, -1
                jnz     IckObjErr
IckObj2:        neg     bx
                add     bx, SegLimit
                cmp     bx, HeapTop
                jbe     IckObjErr
                lea     ax, word ptr [di + 4]
                cmp     ax, word ptr [bx]
                jnz     IckObjErr
IckObjStep:     add     di, word ptr [di]
                jmp     IckObjLoop
IckObjErr: jmp IckFail

IckObjOver:     cmp     cx, HeapGarb
                jnz     IckObjErr

; All objects in the heap are believable.  Now let's test all the
; descriptors in the Array, viz:
;       for ( F = 0, b = SegLimit-6; b -= 8; b > HeapTop)
;               if (b->N == 0)
;                       check b->V == b->S == 0
;                       ++F
;               else
;                       check *(b->N-2) == SegLimit - b
;                       if (b->V == 0) check b->S == 0
;                       else check *(b->V-2) == SegLimit - b + 2
;                            and *(b->V-4) >= b->S
;       check F == ArrayFree
                mov     cx, 0
                mov     dx, 6   ; backoff of first descriptor
IckNameLoop:    mov     bx, SegLimit
                sub     bx, dx
                cmp     bx, HeapTop
                jb      IckNameOver
                mov     di, word ptr [bx]
                cmp     di, 0
                jnz     IckNameActive
                cmp     di, word ptr [bx + 2]
                jnz     IckObjErr
                cmp     di, word ptr [bx + 4]
                jnz     IckObjErr
                inc     cx
                jmp     IckNameStep
IckNameActive:  test    di, 1
                jnz     IckObjErr
                cmp     dx, word ptr [di-2]
                jnz     IckObjErr
                mov     di, word ptr [bx + 2]
                cmp     di, 0
                jnz     IckValue
                cmp     di, word ptr [bx + 4]
                jnz     IckObjErr
                jmp     IckNameStep
IckValue:       mov     ax, dx
                sub     ax, 2
                cmp     ax, word ptr [di - 2]
                jnz     IckObjErr
                mov     ax, word ptr [bx + 4]
                cmp     ax, word ptr [di - 4]
                ja      IckObjErr
IckNameStep:    add     dx, 8
                jmp     IckNameLoop
IckNameOver:
                cmp     cx, ArrayFree
                je      IckSort
                jmp     IckFail

; OK, all the objects have anchors and all the anchors have objects
; and all the descriptors are consistent.  Lastly let us see that
; the top (ArraySize - ArrayFree) entries of the Indirect array
; are nonzero and proper indexes to descriptors.
IckSort:        mov     cx, ArraySize
                sub     cx, ArrayFree
                jcxz    IckExit
                mov     bx, SegLimit
IckSortLoop:    sub     bx, 8
                mov     di, word ptr [bx]
                test    di, 1
                jz      IckSort1
                jmp     IckFail
IckSort1:       cmp     di, 0
                jnz     IckSort2
                jmp     IckFail
IckSort2:       neg     di
                add     di, SegLimit
                cmp     di, HeapTop
                ja      IckSort3
                jmp     IckFail
IckSort3:       cmp     word ptr [di], 0
                jnz     IckSort4
                jmp     IckFail
IckSort4:       loop    IckSortLoop
                jmp     IckExit

; Come here to fail: some inconsistency puts the integrity of the
; entire common segment in doubt.  We just clear it out, making it
; safe to use at the expense of discarding all data.

IckFail:        mov     cx, ds
                lsl     ax, cx
                mov     SegLimit, ax
                mov     HeapTop, ax
                mov     ax, offset EndFixed
                mov     HeapBot, ax
                mov     HeapMid, ax
                xor     ax, ax
                mov     HeapGarb, ax
                mov     ArraySize, ax
                mov     ArrayFree, ax

; back to work
IckExit:        pop     di
                pop     dx
                pop     cx
                pop     bx
                pop     ax
                ret
    Integrity endp
GLOBENV Ends
        END start
