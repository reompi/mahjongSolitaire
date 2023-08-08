
Imports System.IO

Public Class Form1
    Dim tilesCount As Integer
    Dim random As New Random
    Dim filePath As String = Path.Combine(Application.StartupPath, "map.txt")
    Dim field(35, 18, 6) As Vector3i
    Dim hiddenStatePicturebox(35, 18, 6) As Boolean
    Dim alreadyShuffled(35, 18, 6) As Boolean
    Dim flagFirstClick As Boolean = True
    Dim click1, click2 As PictureBox
    Dim tilesShuffled As Integer
    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Me.WindowState = FormWindowState.Maximized
        Dim currentLayer As Integer = 5

        Dim X, Y, Z As Integer
        '««««««««««««««««««LOAD«FILE««««««««««««««««

        While currentLayer > 0
            Using reader As New StreamReader(filePath)

                Y = 0
                X = 0
                Dim line As String = reader.ReadLine
                While line IsNot Nothing

                    X = 0
                    For Each _z As Char In line
                        Z = Asc(_z) - Asc("0")
                        X += 1
                        If Z < currentLayer Then
                            Continue For
                        End If

                        If field(X - 1, Y - 1, currentLayer).cordinateAssigned OrElse field(X - 1, Y, currentLayer).cordinateAssigned OrElse field(X, Y - 1, currentLayer).cordinateAssigned Then
                            field(X, Y, currentLayer).cordinateAssigned = False
                        Else
                            field(X, Y, currentLayer).cordinateAssigned = True
                            tilesCount += 1
                        End If

                    Next
                    Y += 1
                    line = reader.ReadLine()

                End While

            End Using
            currentLayer -= 1
        End While


        '««««««««««««««««««LOAD«MAP««««««««««««««««

        Dim ticker As Integer
        For Z = 5 To 1 Step -1
            For Y = 17 To 1 Step -1
                For X = 35 To 1 Step -1

                    If field(X, Y, Z).cordinateAssigned Then
                        ticker += 1
                        field(X, Y, Z).picturebox = CreatePictureBox(X, Y, Z, ticker)
                        Me.Controls.Add(field(X, Y, Z).picturebox)
                        alreadyShuffled(X, Y, Z) = False
                        hiddenStatePicturebox(X, Y, Z) = False
                        AddHandler field(X, Y, Z).picturebox.Click, AddressOf PictureBox_Click
                    End If

                Next
            Next
        Next

        '««««««««««««««««««SHUFFLE«TILES««««««««««««««««
        Do
            tilesShuffled = 0
            Dim _random As Integer
            Dim currentPiece As Bitmap
            Dim result = checkOpenTiles(alreadyShuffled)
            Dim openTilesX As New List(Of Integer)
            Dim openTilesY As New List(Of Integer)
            Dim openTilesZ As New List(Of Integer)
            Do

                openTilesX = result.item1
                openTilesY = result.item2
                openTilesZ = result.item3
                currentPiece = getRandomImage()
                For pairOfTiles = 1 To 2
                    If openTilesX.Count = 0 Then
                        Exit Do
                    Else
                        _random = random.Next(openTilesX.Count)
                    End If


                    field(openTilesX(_random), openTilesY(_random), openTilesZ(_random)).picturebox.Image = currentPiece
                    DrawBorderInPictureBox(field(openTilesX(_random), openTilesY(_random), openTilesZ(_random)).picturebox, openTilesZ(_random))
                    alreadyShuffled(openTilesX(_random), openTilesY(_random), openTilesZ(_random)) = True
                    tilesShuffled += 1
                    openTilesX.RemoveAt(_random)
                    openTilesY.RemoveAt(_random)
                    openTilesZ.RemoveAt(_random)

                Next
                result = checkOpenTiles(alreadyShuffled)
            Loop Until result.item1.Count < 2
        Loop Until checkAllTilesShuffled() = True




    End Sub
    Private Sub PictureBox_Click(sender As PictureBox, e As EventArgs)
        Dim result = checkOpenTiles(hiddenStatePicturebox)


        Dim click2Coordinates As String = sender.Tag.ToString()
        Dim click2CoordinateArray As String() = click2Coordinates.Split(","c)
        Dim click2X As Integer = Integer.Parse(click2CoordinateArray(0))
        Dim click2Y As Integer = Integer.Parse(click2CoordinateArray(1))
        Dim click2Z As Integer = Integer.Parse(click2CoordinateArray(2))
        If Not result.item4.Contains(field(click2X, click2Y, click2Z)) Then
            Exit Sub
        End If
        If flagFirstClick Then
            click2 = field(click2X, click2Y, click2Z).picturebox
            flagFirstClick = False
            Exit Sub
        End If
        Dim click1Coordinates As String = click2.Tag.ToString()
        Dim click1CoordinateArray As String() = click1Coordinates.Split(","c)
        Dim click1X As Integer = Integer.Parse(click1CoordinateArray(0))
        Dim click1Y As Integer = Integer.Parse(click1CoordinateArray(1))
        Dim click1Z As Integer = Integer.Parse(click1CoordinateArray(2))


        If sender.Name = click2.Name Then
            Exit Sub
        End If

        click1 = click2
        click2 = sender

        If getEqualImage(click1, click2) Then
            click1.Hide()
            click2.Hide()
            hiddenStatePicturebox(click2X, click2Y, click2Z) = True
            hiddenStatePicturebox(click1X, click1Y, click1Z) = True
            flagFirstClick = True
        End If
    End Sub
    Private Function checkAllTilesShuffled() As Boolean

        If tilesCount = tilesShuffled Then
            Return True
        Else Return False
        End If
    End Function
    Private Function checkOpenTiles(hiddenOrShuffled(,,) As Boolean)

        Dim openTilesX As New List(Of Integer)
        Dim openTilesY As New List(Of Integer)
        Dim openTilesZ As New List(Of Integer)
        Dim opentiles As New List(Of Vector3i)

        For Z = 5 To 1 Step -1
            For Y = 17 To 1 Step -1

                For X = 35 To 1 Step -1

                    If field(X, Y, Z).cordinateAssigned AndAlso Not hiddenOrShuffled(X, Y, Z) Then

                        Dim flag As Boolean = True
                        '   ««««««««««««««««««CHECKING«RIGHT«LEFT«««««««««««««««««

                        For tickerSquareY = -1 To 1

                            If field(-2 + X, tickerSquareY + Y, Z).cordinateAssigned AndAlso Not hiddenOrShuffled(-2 + X, tickerSquareY + Y, Z) Then

                                For _tickerSquareY = -1 To 1
                                    If field(2 + X, _tickerSquareY + Y, Z).cordinateAssigned AndAlso Not hiddenOrShuffled(2 + X, _tickerSquareY + Y, Z) Then

                                        flag = False
                                        Exit For
                                    End If
                                Next
                            End If

                        Next

                        If flag Then
                            ' ««««««««««««««««««CHECKING«SQUARE«ABOVE««««««««««««««««««

                            For tickerSquareY = -1 To 1
                                For tickerSquareX = -1 To 1
                                    If field(tickerSquareX + X, tickerSquareY + Y, Z + 1).cordinateAssigned AndAlso Not hiddenOrShuffled(tickerSquareX + X, tickerSquareY + Y, Z + 1) Then
                                        flag = False
                                        Exit For

                                    End If


                                Next
                                If flag = False Then
                                    Exit For
                                End If
                            Next

                            If flag = True Then
                                openTilesX.Add(X)
                                openTilesY.Add(Y)
                                openTilesZ.Add(Z)
                                opentiles.Add(field(X, Y, Z))
                            End If
                        End If
                    End If
                Next
            Next
        Next
        Return (openTilesX, openTilesY, openTilesZ, opentiles)
    End Function





    Public Structure Vector3i
        Public Property cordinateAssigned As Boolean
        Public Property picturebox As PictureBox

    End Structure

    Private Function getEqualImage(image1 As PictureBox, image2 As PictureBox) As Boolean
        Dim bitmap1 As New Bitmap(image1.Image)
        Dim bitmap2 As New Bitmap(image2.Image)

        ' Get the border thickness used in DrawBorderInPictureBox subroutine
        Dim borderThickness As Integer = 10 ' Adjust as per your border thickness

        For x As Integer = borderThickness To bitmap1.Width - borderThickness
            For y As Integer = borderThickness To bitmap1.Height - 1 - borderThickness
                If bitmap1.GetPixel(x, y) <> bitmap2.GetPixel(x, y) Then
                    Return False
                End If
            Next
        Next

        Return True
    End Function



    Public Sub DrawBorderInPictureBox(pictureBox As PictureBox, layer As Integer)
        ' Store the original SizeMode
        Dim originalSizeMode As PictureBoxSizeMode = pictureBox.SizeMode

        ' Set the border color based on the layer
        Dim borderColor As Color

        Select Case layer
            Case 1
                borderColor = Color.FromArgb(173, 83, 79) ' Lighter red
            Case 2
                borderColor = Color.FromArgb(86, 123, 173) ' Lighter blue
            Case 3
                borderColor = Color.FromArgb(96, 173, 83) ' Lighter green
            Case 4
                borderColor = Color.FromArgb(215, 204, 77) ' Lighter yellow
            Case 5
                borderColor = Color.FromArgb(221, 150, 81) ' Lighter orange
            Case Else
                ' Invalid layer or unknown SizeMode, exit the subroutine
                Return
        End Select

        ' Create a new bitmap with the same size as the PictureBox image
        Dim bmp As New Bitmap(pictureBox.Image.Width, pictureBox.Image.Height)

        ' Draw the image onto the bitmap
        Using g As Graphics = Graphics.FromImage(bmp)
            g.DrawImage(pictureBox.Image, Point.Empty)
        End Using

        ' Calculate the thickness of the border (half of the original)
        Dim borderThickness As Integer = 5 ' Half of the original 10-pixel thickness

        ' Create a Pen with the specified border color and width
        Using borderPen As New Pen(borderColor, borderThickness)
            ' Adjust the pen alignment to center the border within the rectangle
            borderPen.Alignment = Drawing2D.PenAlignment.Inset

            ' Draw the border on the bitmap
            Using g As Graphics = Graphics.FromImage(bmp)
                g.DrawRectangle(borderPen, New Rectangle(0, 0, bmp.Width - 1, bmp.Height - 1))
            End Using
        End Using

        ' Update the PictureBox image with the modified bitmap
        pictureBox.Image = bmp

        ' Restore the original SizeMode
        pictureBox.SizeMode = originalSizeMode

        ' Refresh the PictureBox to display the changes
        pictureBox.Refresh()
    End Sub




    Private Function CreatePictureBox(left As Integer, top As Integer, layer As Integer, name As Integer) As PictureBox
        Dim pictureBox As New PictureBox()
        With pictureBox
            .Top = top / 2 * 70 + 100
            .Left = left / 2 * 70 + 300
            .Name = "pictureBox" & name
            .Size = New System.Drawing.Size(70, 70)
            .TabIndex = 0
            .TabStop = False
            .SizeMode = PictureBoxSizeMode.StretchImage
            .Tag = $"{left},{top},{layer}"
        End With
        Return pictureBox
    End Function
    Private Function getRandomImage()

        Dim _random As Integer = random.Next(12)
        Select Case _random
            Case 0
                Return My.Resources.bolas__1_
            Case 1
                Return My.Resources.bolas__2_
            Case 2
                Return My.Resources.bolas__3_
            Case 3
                Return My.Resources.paus__1_
            Case 4
                Return My.Resources.paus__2_
            Case 5
                Return My.Resources.paus__3_
            Case 6
                Return My.Resources.s__1_
            Case 7
                Return My.Resources.s__2_
            Case 8
                Return My.Resources.s__3_
            Case 9
                Return My.Resources.x__1_
            Case 10
                Return My.Resources.x__2_
            Case 11
                Return My.Resources.x__3_
        End Select
        Return Nothing
    End Function
End Class
