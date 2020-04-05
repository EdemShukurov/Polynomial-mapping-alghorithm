Imports Microsoft.FSharp.Collections
Imports System.Numerics
Imports Miller_RabinTestLib
Imports PolynomialMapping

Imports System.Text

Partial Public Class MainWindow
    Inherits Window

    Public Shared EventNameProperty As DependencyProperty

    'Instance of classes from F#
    Private ReadOnly _polynom As WB_Alghorithm.Polynom
    Private ReadOnly _encryption As Encryption.Message
    Private ReadOnly _decryption As Decryption.Message

    'Number elements:
    '_COUNTER is necessary for manual input f1_fk for each prime number
    Private _counter As Integer = 0
    'Multiplying primes number as a PUBLIC KEY
    Private _multiplyPrimes As Integer = 1

    Private _countPrimes As Integer = 0

    'Lists
    Private ReadOnly _polymonialCoefficients As List(Of BigInteger) = New List(Of BigInteger)
    Private ReadOnly _encryptedChars As List(Of BigInteger) = New List(Of BigInteger)
    Private ReadOnly _primeNumbers As List(Of BigInteger) = New List(Of BigInteger)
    Private ReadOnly _asciiChars As List(Of Integer) = New List(Of Integer)

    'constructor
    Public Sub New()

        ' This call is required by the designer.
        InitializeComponent()
        ' Add any initialization after the InitializeComponent() call.
        _polynom = New WB_Alghorithm.Polynom()
        _encryption = New Encryption.Message()
        _decryption = New Decryption.Message()

        MillerRabinResult.Content = ""
        MillerRabinCheckButton.IsEnabled = False
        MillerRabinTxt.IsEnabled = False
    End Sub


    Public Sub Clear()

        'Number elements 
        _counter = 0
        _countPrimes = 0
        _multiplyPrimes = 1

        'Lists
        _primeNumbers.Clear()
        _polymonialCoefficients.Clear()
        _asciiChars.Clear()
        _encryptedChars.Clear()

        'TextBoxes & ListBoxes
        ListPrimesTxt.Clear()
        ListBigPolynom.Clear()
        ListFuncPoly.Clear()
        DecryptedTxt.Clear()
        EncryptedMessage.Clear()
        MessageTxt.Clear()
        MultiplePrimes.Clear()
        CountPrimesTxt.Clear()

        MillerRabinTxt.Clear()
        MillerRabinTxt.IsEnabled = True
        MillerRabinTxt.Focus()

        'Label
        MillerRabinResult.Content = ""

        'Buttons
        EncryptButton.IsEnabled = False
        DecryptButton.IsEnabled = False
        CountPrimesButton.IsEnabled = True
        CountPrimesTxt.IsEnabled = True
        MillerRabinCheckButton.IsEnabled = False


    End Sub

    'Call Miller Rabin Test
    Private Sub MillerRabinCheckButton_Click(sender As Object, e As RoutedEventArgs)
        MillerRabinResult.Content = ""
        Dim number As BigInteger
        If BigInteger.TryParse(MillerRabinTxt.Text, number) Then
            Dim test As MillerRabinTest = New MillerRabinTest()
            Dim result = test.IsPrimeNumber(number)

            If result Then
                'Show prime number in TextBox 
                ListPrimesTxt.Text += number.ToString() + Environment.NewLine


                _primeNumbers.Add(number)
                _multiplyPrimes *= number

                'Create polynomial F for current prime number 
                '_COUNTER is necessary for manual input f1...fk for each prime number
                _polynom.CreatePolynomFunc(number, _counter)

                'Increment counter
                _counter += 1

                'Get f1...fk for current prime number and print them
                Dim lst = _polynom.listPolynoms.First()
                PrintCoefficients(lst, ListFuncPoly)

                ' Reset textbox for input the next prime number
                MillerRabinTxt.Clear()
                MillerRabinTxt.Focus()

                'Check the count prime numbers, by default equals 3
                CheckCountPrimes()
            Else
                Clear()
                ' Warning message
                MillerRabinResult.Content = "INPUT A REALLY PRIME NUMBER"
                MillerRabinTxt.IsEnabled = True
                MillerRabinCheckButton.IsEnabled = True

            End If
        Else
            Clear()
            ' Warning message
            MillerRabinResult.Content = "INPUT AT LEAST A NUMBER"
        End If
    End Sub

    Public Sub CheckCountPrimes()
        If _primeNumbers.Count = _countPrimes Then

            MillerRabinTxt.IsEnabled = False

            'Chiness Algorithm in order to create a big common polynomial
            _polynom.ChinessAlgorithm(_primeNumbers)

            'Get a big common polynomial and Show it as a part of the PUBLIC KEY
            PrintCoefficients(_polynom.bigPolynoms, ListBigPolynom)

            'Add a big common polynomial's coefficients in LIST_POLY_COEFF
            For Each item In _polynom.bigPolynoms
                _polymonialCoefficients.Add(item)
            Next

            'Arrange them in the correct order
            _polymonialCoefficients.Reverse()

            'Demonstrate n = p1*p2...*p_k as a part of the PUBLIC KEY 
            MultiplePrimes.Text = _multiplyPrimes.ToString()

            MessageTxt.IsEnabled = True
            EncryptButton.IsEnabled = True
        Else
            MillerRabinResult.Content = "INPUT A PRIME NUMBER"
        End If

    End Sub

    'Print Coefficients for corresponding element in form
    Public Sub PrintCoefficients(listCoeff As FSharpList(Of BigInteger), txtBox As TextBox)
        Dim coef = ListModule.OfSeq(listCoeff)
        Dim _COUNTER = coef.Count - 1

        For Each item In coef
            Dim print As String = String.Empty

            If item <> 0 Then
                If item > 1 Then print = item.ToString() + " *"

                Select Case _COUNTER
                    Case 0
                        If String.IsNullOrEmpty(print) Then
                            txtBox.Text += "1"
                        End If
                        txtBox.Text += print
                    Case 1
                        txtBox.Text += print + "x + "
                    Case Else
                        txtBox.Text += print + "x" & GetSuperScript(_COUNTER.ToString()) + " + "
                End Select

            End If

            _COUNTER -= 1
        Next
        If txtBox.Text(txtBox.Text.Length - 2) = "+"c Then
            txtBox.Text = txtBox.Text.Remove(txtBox.Text.Length - 3, 3)
        End If
        If txtBox.Text(txtBox.Text.Length - 1) = "*"c Then
            txtBox.Text = txtBox.Text.Remove(txtBox.Text.Length - 2, 2)
        End If
        txtBox.Text += Environment.NewLine + New String("-"c, 50) + Environment.NewLine
    End Sub

    'Encrypt a message
    Private Sub EncryptButton_Click(sender As Object, e As RoutedEventArgs) Handles EncryptButton.Click
        DecryptedTxt.Clear()
        For Each ch In MessageTxt.Text
            'Translate every symbol to ASCII format and add a result in list
            Dim i As Integer = AscW(ch)
            _asciiChars.Add(i)

            'Encrypt symbol
            _encryption.Encrypted(i, _multiplyPrimes, ListModule.OfSeq(_polymonialCoefficients))

            'Add prepared encrypted symbol in list
            _encryptedChars.Add(_encryption.F_m)
        Next

        'Print a result  
        Print_Encrypted_Message()
    End Sub

    'Print encrypted message
    Private Sub Print_Encrypted_Message()
        For Each encrpt In _encryptedChars
            EncryptedMessage.Text += encrpt.ToString
        Next

        'Enable Decrypt Button
        DecryptButton.IsEnabled = True
    End Sub

    'Decryption
    Private Sub DecryptButton_Click(sender As Object, e As RoutedEventArgs) Handles DecryptButton.Click

        Dim list_decrt As List(Of BigInteger) = New List(Of BigInteger)

        'Decrypt every encrypted symbol and add result in list
        For Each encrpt In _encryptedChars
            _decryption.Decrypted(encrpt, ListModule.OfSeq(_primeNumbers), ListModule.OfSeq(_polynom.listPolynoms))

            list_decrt.Add(_decryption.message)
        Next

        'Print a result
        Print_Decrypted_Message(list_decrt)
    End Sub

    'Print decrypted message
    Private Sub Print_Decrypted_Message(list_decrt As List(Of BigInteger))
        Dim decoded As New StringBuilder()

        For Each asciiChar As Integer In list_decrt
            'Translate decrypt symbol to char and give a result in the textbox
            DecryptedTxt.Text += decoded.Append(ChrW(asciiChar)).ToString()
            decoded.Clear()
        Next

        'Disable decrypt button
        DecryptButton.IsEnabled = False
    End Sub

    'Demonstrate a power as a superscript against ^2
    Public Function GetSuperScript(_COUNTER As String) As String
        Dim result = ""
        For index = 0 To _COUNTER.Length - 1
            Select Case _COUNTER(index)
                Case "0"c
                    result += ChrW(SuperScript.n0)
                Case "1"c
                    result += ChrW(SuperScript.n1)
                Case "2"c
                    result += ChrW(SuperScript.n2)
                Case "3"c
                    result += ChrW(SuperScript.n3)
                Case "4"c
                    result += ChrW(SuperScript.n4)
                Case "5"c
                    result += ChrW(SuperScript.n5)
                Case "6"c
                    result += ChrW(SuperScript.n6)
                Case "7"c
                    result += ChrW(SuperScript.n7)
                Case "8"c
                    result += ChrW(SuperScript.n8)
                Case "9"c
                    result += ChrW(SuperScript.n9)
                Case Else
                    result += ""
            End Select

        Next
        Return result
    End Function

    'SuperScript enumeration
    Public Enum SuperScript As Integer
        n0 = &H2070
        n1 = &HB9
        n2 = &HB2
        n3 = &HB3
        n4 = &H2074
        n5 = &H2075
        n6 = &H2076
        n7 = &H2077
        n8 = &H2078
        n9 = &H2079
    End Enum

    ' Clear Button
    Private Sub ClearButton_Click(sender As Object, e As RoutedEventArgs) Handles ClearButton.Click
        '_POLY.listPolynoms = FSharpList(Of FSharpList(Of BigInteger) BigInteger).Empty
        '_POLY.bigPolynoms = FSharpList(Of BigInteger) BigInteger).Empty
        Dim clearlist As FSharpList(Of BigInteger) = FSharpList(Of BigInteger).Empty
        Dim clearlist_list As FSharpList(Of FSharpList(Of BigInteger)) = FSharpList(Of FSharpList(Of BigInteger)).Empty

        _polynom.bigPolynoms = clearlist
        _polynom.listPolynoms = clearlist_list
        _polynom.listPrimeNumbers = clearlist

        _decryption.message = 0
        _encryption.F_m = 0
        Clear()

    End Sub

    Private Sub CountPrimesButton_Click(sender As Object, e As RoutedEventArgs) Handles CountPrimesButton.Click
        _countPrimes = Int32.Parse(CountPrimesTxt.Text)
        CountPrimesButton.IsEnabled = False
        CountPrimesTxt.IsEnabled = False

        MillerRabinTxt.IsEnabled = True
        MillerRabinCheckButton.IsEnabled = True
    End Sub
End Class
