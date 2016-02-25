Module output
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
  Use printMod       ! from libBP
  Use globals
! Force declaration of all variables
  Implicit None
! Privacy of variables/functions/subroutines
  Private
! Public Subroutines
  Public :: outputDataPoints
  Public :: outputSummary

  Contains

! ---------------------------------------------------------------------------------------------------
! Save to specific file
! ---------------------------------------------------------------------------------------------------

  Subroutine outputDataPoints(fileName,pointsIn,pointsFit)
! Saves the eam file to the output directory
    Implicit None   ! Force declaration of all variables
! Private variables
    Character(*) :: fileName
    Real(kind=DoubleReal), Dimension(:,:) :: pointsIn
    Real(kind=DoubleReal), Dimension(:,:) :: pointsFit
! -----
    Character(len=255) :: filePath
    Integer(kind=StandardInteger) :: i
! Only on root process
    If(mpiProcessID.eq.0)Then
      filePath = Trim(outputDirectory)//"/"//fileName
      Open(UNIT=9999,FILE=Trim(filePath))
! Loop through data points
      Do i=1,size(pointsIn,1)
        write(9999,"(E17.10,A1,E17.10,A1,E17.10)") pointsIn(i,1),",",&
        pointsIn(i,2),",",pointsFit(i,2)
      End Do
    End If
! Close file
    Close(9999)
  End Subroutine outputDataPoints


  Subroutine outputSummary(fileName,outputFitText,coefficients)
  ! Saves the eam file to the output directory
      Implicit None   ! Force declaration of all variables
  ! Private variables
      Character(*) :: fileName
      Character(*) :: outputFitText
      Real(kind=DoubleReal), Dimension(:) :: coefficients
  ! -----
      Character(len=255) :: filePath
      Integer(kind=StandardInteger) :: i
  ! Only on root process
      If(mpiProcessID.eq.0)Then
! Print to Terminal
        print *,"Summary:"
        Call printBR()
        print *,trim(outputFitText)
        Do i=1,size(coefficients,1)
          print "(I8,A1,E17.10)", i," ",coefficients(i)
        End Do
! Write to file
        filePath = Trim(outputDirectory)//"/"//fileName
        Open(UNIT=9999,FILE=Trim(filePath))
  ! Loop through data points
        write(9999,*) trim(outputFitText)
        Do i=1,size(coefficients,1)
          write(9999,"(I8,A1,E17.10)") i," ",coefficients(i)
        End Do
      End If
  ! Close file
      Close(9999)
  End Subroutine outputSummary

End Module output
