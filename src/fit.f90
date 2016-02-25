Module fit
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
  Use plotTypes      ! from libBP
  Use plot           ! from libBP
  Use strings        ! from libBP
  Use general        ! from libBP
  Use regression     ! from libBP
  Use basicMaths     ! from libBP
  Use splinesFitting ! from libBP
  Use calcFunctions  ! from libBP
  Use globals
  Use output
! Force declaration of all variables
  Implicit None
! Privacy of variables/functions/subroutines
  Private
! Public Subroutines
  Public :: runFit

  Contains

! ---------------------------------------------------------------------------------------------------
! Save to specific file
! ---------------------------------------------------------------------------------------------------

  Subroutine runFit()
! Runs fitting
    Implicit None   ! Force declaration of all variables
! Private variables
!    Character(len=255) :: fileRow, fileRowU
!    Integer(kind=StandardInteger) :: i, j
!    Integer(kind=StandardInteger) :: rows, columns
    print *,""
    print *,"Fitting to Data..."
    print *,""
    Call runFit_Process()
  End Subroutine runFit


  Subroutine runFit_Process()
! Runs fitting process
    Implicit None   ! Force declaration of all variables
! Private variables
    Real(kind=DoubleReal), Dimension(1:fitDataRows,1:2) :: pointsIn
    Real(kind=DoubleReal), Dimension(1:fitDataRows,1:2) :: pointsFit
    Real(kind=DoubleReal), Dimension(1:(5*fitDataRows),1:2) :: pointsChart
    Real(kind=DoubleReal) :: rssVal
    Integer(kind=StandardInteger) :: i, tempInt
    Real(kind=DoubleReal), Dimension(1:3) :: coefficientsP2
    Real(kind=DoubleReal), Dimension(1:4) :: coefficientsP3
    Real(kind=DoubleReal), Dimension(1:2) :: coefficientsE1
    Real(kind=DoubleReal), Dimension(1:4) :: coefficientsE2
    Real(kind=DoubleReal), Dimension(1:6) :: coefficientsE3
    Real(kind=DoubleReal), Dimension(1:4) :: coefficientsBM
    Real(kind=DoubleReal) :: xMin, xMax
    Real(kind=DoubleReal) :: chartPointsInc
    Integer(kind=StandardInteger) :: chartPointsCount
    Character(len=128) :: outputFitText
    Character(len=30) :: fileName
    Type(plotData) :: fitPlotData
    Character(Len=16) :: label
    Character(len=16) :: prefix
! Sort out data
    Do i=1,fitDataRows
      pointsIn(i,1) = fitData(i,1)
      pointsIn(i,2) = fitData(i,2)
    End Do
    xMin = pointsIn(1,1)
    xMax = pointsIn(fitDataRows,1)
    chartPointsCount = 5*fitDataRows
    chartPointsInc = 1.0D0*(xMax-xMin)/(chartPointsCount-1)

! ------------------------------
! POLY2
! ------------------------------
    If(fitType(1:5).eq."POLY2")Then
! Fit
      coefficientsP2 = PolyFit(pointsIn,2)
! Make fit points
      Do i=1,fitDataRows
        pointsFit(i,1) = pointsIn(i,1)
        pointsFit(i,2) = CalcPolynomial(coefficientsP2, pointsIn(i,1))
      End Do
      Do i=1,chartPointsCount
        pointsChart(i,1) = xMin+(i-1)*chartPointsInc
        pointsChart(i,2) = CalcPolynomial(coefficientsP2, pointsChart(i,1))
      End Do
! Output summary
      outputFitText = "F(x) = c(1)+c(2)x+c(3)x^2"
      Call outputSummary("summary.txt",outputFitText,coefficientsP2)
    End If
! ------------------------------
! POLY3
! ------------------------------
    If(fitType(1:5).eq."POLY3")Then
! Fit
      coefficientsP3 = PolyFit(pointsIn,3)
! Make fit points
      Do i=1,fitDataRows
        pointsFit(i,1) = pointsIn(i,1)
        pointsFit(i,2) = CalcPolynomial(coefficientsP3, pointsIn(i,1))
      End Do
      Do i=1,chartPointsCount
        pointsChart(i,1) = xMin+(i-1)*chartPointsInc
        pointsChart(i,2) = CalcPolynomial(coefficientsP3, pointsChart(i,1))
      End Do
! Output summary
      outputFitText = "F(x) = c(1)+c(2)x+c(3)x^2+c(4)x^3"
      Call outputSummary("summary.txt",outputFitText,coefficientsP3)
    End If
! ------------------------------
! EXP1
! ------------------------------
    If(fitType(1:4).eq."EXP1")Then
! Fit
      coefficientsE1 = ExpFit(pointsIn,1)
! Make fit points
      Do i=1,fitDataRows
        pointsFit(i,1) = pointsIn(i,1)
        pointsFit(i,2) = ExpCalc(pointsIn(i,1), coefficientsE1)
      End Do
      Do i=1,chartPointsCount
        pointsChart(i,1) = xMin+(i-1)*chartPointsInc
        pointsChart(i,2) = ExpCalc(pointsChart(i,1), coefficientsE1)
      End Do
! Output summary
      outputFitText = "F(x) = c(1)exp(c(2)x)"
      Call outputSummary("summary.txt",outputFitText,coefficientsE1)
    End If
! ------------------------------
! EXP2
! ------------------------------
    If(fitType(1:4).eq."EXP2")Then
! Fit
      coefficientsE2 = ExpFit(pointsIn,2)
! Make fit points
      Do i=1,fitDataRows
        pointsFit(i,1) = pointsIn(i,1)
        pointsFit(i,2) = ExpCalc(pointsIn(i,1), coefficientsE2)
      End Do
      Do i=1,chartPointsCount
        pointsChart(i,1) = xMin+(i-1)*chartPointsInc
        pointsChart(i,2) = ExpCalc(pointsChart(i,1), coefficientsE2)
      End Do
! Output summary
      outputFitText = "F(x) = c(1)exp(c(2)x)+c(3)exp(c(4)x)"
      Call outputSummary("summary.txt",outputFitText,coefficientsE2)
    End If
! Calculate RSS
    rssVal = RSSPoints(pointsIn, pointsFit)
! ------------------------------
! Birch Murnaghan
! ------------------------------
    If(fitType(1:2).eq."BM")Then
! Fit
      coefficientsBM = BirchMurnFit(pointsIn)
! Make fit points
      Do i=1,fitDataRows
        pointsFit(i,1) = pointsIn(i,1)
        pointsFit(i,2) = BirchMurnCalc(pointsIn(i,1),coefficientsBM)
      End Do
      Do i=1,chartPointsCount
        pointsChart(i,1) = xMin+(i-1)*chartPointsInc
        pointsChart(i,2) = BirchMurnCalc(pointsChart(i,1),coefficientsBM)
      End Do
! Output summary
      outputFitText = "eta = ((1.0D0*volume)/(1.0D0*V))**(1.0D0/3.0D0)  "
      outputFitText = outputFitText//"energy = E+(9.0D0/16.0D0)*(B*V)*"
      outputFitText = outputFitText//"((eta**2-1.0D0)**2)*(6.0D0+BP*(eta**2-1.0D0)-4.0D0*eta**2)"
      Call outputSummary("summary.txt",outputFitText,coefficientsBM)
    End If
! Calculate RSS
    rssVal = RSSPoints(pointsIn, pointsFit)



! ------------------------------
! RSS
! ------------------------------
    print *,"RSS: ",rssVal
! ------------------------------
! Output Data Points
! ------------------------------
    Call outputDataPoints("fitpoints.csv",pointsIn,pointsFit)
! ------------------------------
! Make Chart
! ------------------------------
    fileName = "chart"
    Call plotInit(fitPlotData)
    fitPlotData%tempDirectory = trim(tempDirectory)
    fitPlotData%outputDirectory = trim(outputDirectory)
    fitPlotData%outputName = trim(fileName)
    fitPlotData%title = "Plot"
    fitPlotData%xAxis = "x"
    fitPlotData%yAxis = "f(x)"
    fitPlotData%cleanPyFile = .true.
    fitPlotData%cleanGpFile = .true.
    fitPlotData%dataFile = .false.
    Call plotAdd(fitPlotData, pointsIn, "", "", 1, fitDataRows, 1, 2)
    Call plotAdd(fitPlotData, pointsChart, "", "", 1, chartPointsCount, 1, 2)
    fitPlotData%fittingSummaryFile = .true.
    fitPlotData%gpLinestyle(1) = "points"
    fitPlotData%gpLinestyle(2) = "lines"
    fitPlotData%label(1) = "Data Points"
    fitPlotData%label(2) = "Fit"
    fitPlotData%gplOutType = "PNG"
    fitPlotData%width = 1080
    fitPlotData%height = 720
    Call plotMake(fitPlotData)
    fitPlotData%outputName = trim(fileName)//"1"
    fitPlotData%gplOutType = "EPS"
    fitPlotData%width = 8
    fitPlotData%height = 6
    Call plotMake(fitPlotData)

  End Subroutine runFit_Process











End Module fit
