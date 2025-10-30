' Image to QOI
' By Aaditya Parashar
' References: https://en.wikipedia.org/wiki/QOI_(image_format)

' Bug: Diff values are 1 value off from the correct byte

$Console:Only
Dim As _MEM M, M2
For I = 1 To _CommandCount
    IMG& = _LoadImage(Command$(I), 32)
    If IMG& >= -1 Then Print "Cannot open Image: " + Command$(I): _Continue

    ' Code to Transfer Image Data to I$
    M = _MemImage(IMG&)
    I$ = String$(M.SIZE, 0)
    M2 = _Mem(_Offset(I$), M.SIZE)
    _MemCopy M, M.OFFSET, M.SIZE To M2, M2.OFFSET
    _MemFree M
    _MemFree M2

    Open ChangeFileExtension(Command$(I), ".qb.qoi") For Output As #1
    O$ = _Deflate$(Compress$(I$, _Width(IMG&), _Height(IMG&)))
    Print #1, O$;
    Close #1
    _FreeImage IMG&
    Print "Done: " + Command$(I)
Next I
System
Function ChangeFileExtension$ (F$, E$)
    P~& = _InStrRev(F$, ".")
    If P~& = 0 Then ChangeFileExtension$ = F$ + E$ Else ChangeFileExtension$ = Left$(F$, P~& - 1) + E$
End Function
Function Compress$ (I$, W~&, H~&)
    Dim As _Unsigned Long I ' Current Color Index
    Dim As _Unsigned Long B_OFFSET, ByteBufferSize: Dim As String * 5 ByteBuffer ' Used in Byte Packing
    Dim As _Unsigned _Byte R, G, B, A ' Current Pixel
    Dim As _Unsigned _Byte PR, PG, PB, PA ' Previous Pixel
    Dim As _Unsigned Long CurrentPixel, PreviousPixel
    Dim As _Unsigned _Byte dR, dG, dB ' Pixel Differences
    Dim As _Unsigned _Bit * 6 H ' Hash Index
    Dim As _Unsigned _Byte L ' Run Length Encoding
    Dim As _Unsigned _Bit * 3 J: J = 0 ' Used in Bit Packing
    Dim As _Unsigned Long HT(0 To 63) ' Hash Table of size 2 ^ ( Number of Bits of H )
    PA = 255: PreviousPixel = 255 ' B: 0 G: 0 R: 0 A: 255

    B$ = String$(65536, 0) ' Output
    B_OFFSET = 1

    For I = 1 To Len(I$) Step 4
        B = Asc(I$, I): G = Asc(I$, I + 1): R = Asc(I$, I + 2): A = Asc(I$, I + 3) ' BGRA (windows)
        CurrentPixel = _SHL(B, 24) Or _SHL(G, 16) Or _SHL(R, 8) Or A

        If CurrentPixel = PreviousPixel And L < 62 Then ' Check for Run Length, 62 -> &B11 + &H3D = &HFD
            L = L + 1
            _Continue
        End If

        H = (R * 3 + G * 5 + B * 7 + A * 11) And 16383
        If L > 0 Then ' Encode for Run Length
            I = I - 4 ' Go Back
            ' Code for QOI_OP_RUN
            Asc(B$, B_OFFSET) = _SHL(3, 6) Or (L - 1)
            B_OFFSET = B_OFFSET + 1
            L = 0
            _Continue
        ElseIf HT(H) = CurrentPixel Then ' Encode for Hash Table
            ' Code for QOI_OP_INDEX is 0, so we can neglect ORing that
            Asc(B$, B_OFFSET) = H
            B_OFFSET = B_OFFSET + 1
        Else
            ' Calculate dR, dG, dB
            dR = R - PR + 2 ' bias of 2
            dG = G - PG + 2
            dB = B - PB + 2
            If dR < 4 And dG < 4 And dB < 4 And A = PA Then
                Asc(B$, B_OFFSET) = _SHL(1, 6) Or _SHL(dR, 4) Or _SHL(dG, 2) Or dB ' Code for QOI_OP_DIFF
                B_OFFSET = B_OFFSET + 1
            ElseIf A = PA Then
                dG = dG + 30 ' 32 - 2 (from previous)
                dR = (dR - 2) - (dG - 32) + 8 ' bias of 8
                dB = (dB - 2) - (dG - 32) + 8
                If dR < 16 And dG < 64 And dB < 16 And dA = 0 Then
                    Asc(B$, B_OFFSET) = _SHL(2, 6) Or dG ' Code for QOI_OP_LUMA
                    Asc(B$, B_OFFSET + 1) = _SHL(dR, 4) Or dB
                    B_OFFSET = B_OFFSET + 2
                ElseIf A = PA Then
                    Asc(B$, B_OFFSET) = 254 ' Code for QOI_OP_RGB
                    Asc(B$, B_OFFSET + 1) = R
                    Asc(B$, B_OFFSET + 2) = G
                    Asc(B$, B_OFFSET + 3) = B
                    B_OFFSET = B_OFFSET + 4
                Else
                    Asc(B$, B_OFFSET) = 255 ' Code for QOI_OP_RGBA
                    Asc(B$, B_OFFSET + 1) = R
                    Asc(B$, B_OFFSET + 2) = G
                    Asc(B$, B_OFFSET + 3) = B
                    Asc(B$, B_OFFSET + 4) = A
                    B_OFFSET = B_OFFSET + 5
                End If
            End If
        End If
        HT(H) = CurrentPixel
        PB = B: PG = G: PR = R: PA = A: PreviousPixel = CurrentPixel

        If Len(B$) < B_OFFSET + 8 Then B$ = B$ + String$(65536, 0) ' Increase Size of Output Buffer if needed
    Next I
    If L > 0 Then
        Asc(B$, B_OFFSET) = _SHL(3, 6) Or (L - 1)
        B_OFFSET = B_OFFSET + 1
    End If
    O$ = Left$(B$, B_OFFSET)
    Compress$ = "qoif" + ReverseMKL$(W~&) + ReverseMKL$(H~&) + Chr$(4) + Chr$(0) + O$ + String$(7, 0) + Chr$(1)
    B$ = "": O$ = ""
End Function
Function ReverseMKL$ (__L~&)
    Dim __RMKL As String
    __RMKL = String$(4, 0)
    Asc(__RMKL, 1) = _SHR(__L~&, 24)
    Asc(__RMKL, 2) = _SHR(__L~&, 16)
    Asc(__RMKL, 3) = _SHR(__L~&, 8)
    Asc(__RMKL, 4) = __L~&
    ReverseMKL$ = __RMKL
End Function
