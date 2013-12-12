'================================================================================
' Name        : 3DMark Calculator (Console) v1.A *10/2012*
' Author      : El Genieben (twitter.com/genieben)
' Description : Handy tool to calculate final score in 3DMark benchmarks give
'               the scores of the subtests. Use this to check for fake scores.
'================================================================================
#RESOURCE ICON,    100, "calc.ico"
#COMPILE EXE
#DIM ALL
$INCLUDE "WIN32API.INC"

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
GLOBAL hPBCC&
GLOBAL SelectedBench    AS LONG
GLOBAL Selected2006     AS LONG
GLOBAL SelectedVantage  AS LONG
GLOBAL Selected2011     AS LONG
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
FUNCTION FLOOR (X AS DOUBLE) AS DOUBLE  'Rounds downward anything that is not an integer
   DIM TMPVAR AS DOUBLE
   DIM OUTVAR AS DOUBLE
   X = X - 1 'remove this and you have CEILING instead of FLOOR
   TMPVAR = INT(X * 1)
   OUTVAR = (TMPVAR + IIF(X = TMPVAR, 0, 1)) / 1
   FUNCTION= OUTVAR
END FUNCTION
SUB pause
    DIM an$
WHILE INSTAT=0
WEND
an$ = INKEY$
        IF an$ = CHR$(27) THEN ' ESCape key pressed.
            END
        ELSE                   ' any other key pressed.
            CLS
            PrintAppHeader
            PrintMenu
        END IF
END SUB
SUB CenterWindow (BYVAL hAnyWnd AS LONG)
  DIM WndRect AS RECT
  DIM x       AS LONG
  DIM y       AS LONG
  GetWindowRect hAnyWnd, WndRect
  x = (GetSystemMetrics(%SM_CXSCREEN)-(WndRect.nRight-WndRect.nLeft))\2
  y = (GetSystemMetrics(%SM_CYSCREEN)-(WndRect.nBottom-WndRect.nTop+GetSystemMetrics(%SM_CYCAPTION)))\2
  SetWindowPos hAnyWnd, %NULL, x, y, 0, 0, %SWP_NOSIZE OR %SWP_NOZORDER
END SUB
SUB PrintAppHeader
        COLOR 14, 9
        STDOUT CHR$(32, 201) + STRING$(36, 205) + CHR$(187, 32)
        STDOUT CHR$(32, 186) + " SysComp Calc v0.2 by GENiEBEN      " + CHR$(186, 32)
        STDOUT CHR$(32, 200) + STRING$(36, 205) + CHR$(188, 32)
        COLOR 1, 7
END SUB
SUB PrintMENU
        LOCAL s  AS STRING
        LOCAL s2 AS STRING
        LOCAL s3 AS STRING
        LOCAL s4 AS STRING
        LOCAL Subtest1      AS DOUBLE
        LOCAL Subtest2      AS DOUBLE
        LOCAL Subtest3      AS DOUBLE
        LOCAL Subtest4      AS DOUBLE
        LOCAL Subtest5      AS DOUBLE
        LOCAL Subtest6      AS DOUBLE
        LOCAL Subtest7      AS DOUBLE
        LOCAL Subtest8      AS DOUBLE
        LOCAL Subtest9      AS DOUBLE
        LOCAL Subtest00     AS LONG
        LOCAL Subtest01     AS LONG
        LOCAL CPU_SCORE     AS STRING
        LOCAL AMP_SCORE     AS STRING
        LOCAL TOT_SCORE     AS STRING
           '
        STDOUT CHR$(32, 201) + STRING$(36, 205) + CHR$(187, 32)
        STDOUT CHR$(32, 186) + " [1] Calculate Total Score          " + CHR$(186, 32)
        STDOUT CHR$(32, 186) + " [2] Calculate CPU Score            " + CHR$(186, 32)
        STDOUT CHR$(32, 186) + " [3] Calculate AMP Score            " + CHR$(186, 32)
        STDOUT CHR$(32, 200) + STRING$(36, 205) + CHR$(188, 32)
        STDOUT
        COLOR 15, 0
        INPUT  "Input a number and press Enter: ", s
        STDOUT

        SelectedBench = VAL(s)
        SELECT CASE SelectedBench
            CASE < 1, > 3 'Invalid Selection
                CLS
                PrintAppHeader
                PrintMenu
            CASE 1
                 CLS
                 PrintAppHeader
                 COLOR 15, 2
                 STDOUT "Input your subtests score               "
                 COLOR 15, 0
                 INPUT "CPU Score   = ", Subtest00
                 INPUT "AMP Score   = ", Subtest01
                 COLOR 15, 6
                 TOT_SCORE = FORMAT$(ROUND(1.0 / (0.949999988079071 / Subtest01 + 0.05000000074505806 / Subtest00),0))
                 STDOUT "TOTAL SCORE = " & TOT_SCORE
                 COLOR 15, 0
                 STDOUT
                 STDOUT "Press any key to return at menu...     "
                 pause
                 CLS
                 PrintAppHeader
                 PrintMenu
            CASE 2
                 CLS
                 PrintAppHeader
                 COLOR 15, 2
                 STDOUT "Input your subtests score               "
                 COLOR 15, 0
                 INPUT "Stage 1 (2D CPU-EX): Score = ", Subtest1
                 INPUT "Stage 2 (3D CPU-EX): Score = ", Subtest2
                 INPUT "Stage 3 (2D CPU-IM): Score = ", Subtest3
                 INPUT "Stage 4 (CPU 3DPMo): Score = ", Subtest4
                 INPUT "Stage 5 (CPU NBODY): Score = ", Subtest5
                 COLOR 15, 6
                 CPU_SCORE = FORMAT$((Subtest1 * 0.1 + Subtest2 * 0.25 + Subtest3 * 0.25 + Subtest4 * 0.1 + Subtest5 * 0.03) * 1000.0,"#")
                 STDOUT "CPU SCORE                  = " & CPU_SCORE
                 COLOR 15, 0
                 STDOUT
                 STDOUT "Press any key to return at menu...     "
                 pause
                 CLS
                 PrintAppHeader
                 PrintMenu
            CASE 3
                 CLS
                 PrintAppHeader
                 COLOR 15, 2
                 STDOUT "Input your subtests score               "
                 COLOR 15, 0
                 INPUT "Stage 6 (2D AMP-EX): Score = ", Subtest6
                 INPUT "Stage 7 (3D AMP-EX): Score = ", Subtest7
                 INPUT "Stage 8 (AMPMatMul): Score = ", Subtest8
                 INPUT "Stage 9 (AMP 3DPMo): Score = ", Subtest9
                 COLOR 15, 6
                 AMP_SCORE = FORMAT$((Subtest6 * 0.1 + Subtest7 * 0.05 + ((Subtest8 * 2) * 0.25) + Subtest9 * 0.001) * 100.0,"#")
                 STDOUT "ESTIMATED AMP SCORE        = " & AMP_SCORE
                 COLOR 15, 0
                 STDOUT
                 STDOUT "Press any key to return at menu...     "
                 pause
                 CLS
                 PrintAppHeader
                 PrintMenu
        END SELECT
END SUB

FUNCTION PBMAIN () AS LONG
  '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
   hPBCC = CONSHNDL
   DeleteMenu GetSystemMenu (hPBCC,0), %SC_RESTORE,  %MF_BYCOMMAND OR %MF_DISABLED
   'DeleteMenu GetSystemMenu (hPBCC,0), %SC_CLOSE,    %MF_BYCOMMAND OR %MF_DISABLED
   'DeleteMenu GetSystemMenu (hPBCC,0), %SC_MINIMIZE, %MF_BYCOMMAND OR %MF_DISABLED
   DeleteMenu GetSystemMenu (hPBCC,0), %SC_MAXIMIZE, %MF_BYCOMMAND OR %MF_DISABLED
  '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        CenterWindow hPBCC
        PrintAppHeader
        PrintMenu
        pause
END FUNCTION
