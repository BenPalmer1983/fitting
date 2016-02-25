Module input
! --------------------------------------------------------------!
! Ben Palmer, University of Birmingham
! Module: loadData
! Updated: 18th February 2016
! --------------------------------------------------------------!
! Description:
! Output data
! --------------------------------------------------------------!

! Setup Modules
  Use kinds          ! from libBP
  Use strings        ! from libBP
  Use general        ! from libBP
  Use globals
! Force declaration of all variables
  Implicit None
! Privacy of variables/functions/subroutines
  Private
! Public Subroutines
  Public :: readInput

  Contains

! ---------------------------------------------------------------------------------------------------
! Save to specific file
! ---------------------------------------------------------------------------------------------------

  Subroutine readInput()
! Saves the eam file to the output directory
    Implicit None   ! Force declaration of all variables
! Private variables
    Character(len=255) :: fileRow, fileRowU
    Integer(kind=StandardInteger) :: i, j
! Read through input file
    Do i=1,inputFileRows
      fileRow = inputFileData(i)
      fileRowU = StrToUpper(fileRow)
      If(fileRowU(1:3).eq."FIT")Then
        fitType = adjustl(trim(fileRowU(4:12)))
      End If
      If(fileRowU(1:4).eq."FILE")Then
        fitDataFile = adjustl(trim(fileRow(5:128)))
      End If
    End Do
! Read CSV
    Call readCSV(trim(fitDataFile), ",", fitData, fitDataRows, fitDataColumns)
! Move to "Output" Module
    print *,"Fit type:       ",trim(fitType)
    print *,"Fit data file:  ",trim(fitDataFile)
    print *,"Data rows:      ",fitDataRows
    print *,"Data columns:   ",fitDataColumns
  End Subroutine readInput

End Module input
