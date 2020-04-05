namespace PolynomialMapping

/// ===========================================
/// Common functions
/// ===========================================
module private Functions = 

    open System.Numerics
    open Microsoft.FSharp.Collections
    open MathNet.Numerics.LinearAlgebra

    let rand = new System.Random()
    
    /// <summary>
    /// Euclidean algorithm 
    /// </summary> 
    /// <param name="a">the first number</param>
    /// <param name="b">the second number</param>
    /// <returns> GCD(a,b) </returns>
    let rec gcd (a : bigint) (b : bigint) =
        if b = 0I then a
        else gcd b (a % b)

    /// <summary>
    /// Factorial: n!
    /// </summary>
    /// <param name="">number</param>
    /// <returns> factorial from n </returns>
    let rec (!) = fun n -> 
                    match n with 
                    | 0 | 1 -> 1
                    | _ -> n * !(n-1)  

    /// <summary>
    /// The number of combinations of n elements by k ones
    /// </summary>
    /// <param name="n"> n elements </param>
    /// <param name="k"> k elements </param>
    /// <returns> The number of Combinations </returns>
    let C k n = !n /(!k * !(n - k))                                           

    /// <summary>
    /// Multiplying of two lists, the result is a 2d-array
    /// </summary> 
    /// <param name="lst1"> mutable multiplier, as a result of FuncInitResList function </param>
    /// <param name="lst2"> immutable multiplier </param>
    /// <returns> 2d-array </returns>
    let MultipleLists_Arr (lst1 : bigint list) 
                          (lst2 : bigint list) = 
                            Array2D.init lst1.Length lst2.Length                                 
                                (fun x y -> lst1.[x] * lst2.[y])                    
    

    /// <summary>
    /// Create list of coefficients via accumalate corresponding 2d-array's elements  
    /// </summary> 
    /// <param name="arr"> 2d-array, as a result of MultipleLists_Arr function </param>
    /// <param name="result"> empty list </param>
    /// <returns> list of F-coefficients, but not F_k </returns>
    let FuncInitResList (arr : bigint [,], result : bigint []) = 

        let mutable _i = 0
        let mutable _j = (Array2D.length2 arr) - 1
        
        while (_i < (Array2D.length1 arr) && _j > -1) do

            for j in 0 .. _j do
                result.[_i+j] <- result.[_i+j] + arr.[_i, j]

            for i in _i + 1 .. (Array2D.length1 arr) - 1 do
                result.[i + _j] <- result.[i + _j] + arr.[i, _j ]
        

            _i <- _i + 1 
            _j <- _j - 1
        
        result     

    /// <summary>
    /// Define residue Z(phi(p))
    /// </summary> 
    /// <param name="p">prime number</param>
    /// <returns>list of the numbers as Z </returns>
    let DefineResidue_Zphi (p : bigint) =
        let mutable Z_phi : bigint list = []
        let phi = p-1I
        for i in 1I .. 2I .. phi-1I do
            if (gcd phi i = 1I && i < 8I) then Z_phi <- i :: Z_phi
        
        Z_phi
    
    /// <summary>
    /// Identify matrix 
    /// </summary> 
    let mutable E = matrix [ [ 1.0; 0.0]; [ 0.0; 1.0] ]

    /// <summary>
    /// Extended Euclidean algorithm, ax = 1 mod b
    /// </summary> 
    /// <param name="a">the number, which need to find the corresponding reverse number for</param>
    /// <param name="b">module</param>
    /// <returns>reverse a (a^-1)</returns>
    let rec Gcd_ex (a : bigint) (b : bigint) =
        if(a % b <> 0I) 
            then
                let arr = matrix [ [ 0.0; 1.0];[ 1.0; float (-a/b)]]
                E <- E * arr 
                Gcd_ex b (a % b)          
        else
            bigint E.[0,1]
    

    /// <summary>
    /// Sign check for reverse element
    /// </summary> 
    /// <param name="a">reverse element</param>
    /// <param name="p">module</param>
    /// <returns>unsigned number</returns>
    let Check_negative (a : bigint) (p : bigint) =
        E <- matrix [ [ 1.0; 0.0]; [ 0.0; 1.0] ]
        if a < 0I
            then
                let mutable positive_a = a
                while positive_a < 0I do
                    positive_a <- positive_a + p    
                positive_a
        else a


    /// <summary>
    /// List of all polinomials and their coefficients
    /// </summary> 
    /// <param name="z">residue ring over field Z(p)</param>
    /// <param name="z_phi">residue ring over field Z(p-1)</param>
    /// <returns>List of coefficients for f_k</returns>
    let Func_List_f (k : bigint) (z : bigint list) (z_phi : bigint list) =
        let mutable lst : bigint list list = [] 
        for f in 1I .. k do
        
            // ----   f = (a*x + b) ^ e + d   -----
        
            let a = z.[rand.Next(1, z.Length)]                              // a belongs Z, but a <> 0
            let b = z.[rand.Next(0, z.Length)]                              // b belongs Z
            let e = z_phi.[rand.Next(0, z_phi.Length)]                      // e belongs Z_phi, invertible element
            let d = z.[rand.Next(0, z.Length)]                              // d belongs Z
        
            let arr_f  = [a; b; e; d]
        
            lst <- arr_f :: lst
        lst

    /// <summary>
    /// List of coefficients for f_k, last polynomial: (ax + b)^e + d. It must be calculated first
    /// </summary> 
    /// <param name="p">prime number</param>
    /// <param name="a">coefficent x^1</param>
    /// <param name="b">coefficent x^0</param>
    /// <param name="e">power (ax+b), invertible</param>
    /// <param name="d"></param>
    /// <returns>List of coefficients for f_k, last polynomial</returns>
    let Func_Fk_coeff p a b power d= 
        let coef : bigint array = Array.zeroCreate (int power + 1)
        for t in 0 .. coef.Length - 1 do
            coef.[t] <- BigInteger(C t (coef.Length-1)) * BigInteger.Pow( a, coef.Length - t - 1) * BigInteger.Pow (b, t)

        coef.[coef.Length - 1] <- coef.[coef.Length - 1] + d
        coef |> Array.map (fun x -> x % p) |> Array.toList  

    /// <summary>
    /// The method 'Brute Force'
    /// </summary>
    /// <param name="lst"> n elements </param>
    /// <param name="p"> primary key, mod </param>
    /// <param name="c"> message </param>
    /// <returns> The list of x </returns>
    let Brute_Force (lst : bigint list) p c  = 
        let mutable _power = 0
        let mutable _x = 0 
        let mutable bf_X : bigint = -1I
        
        let mutable temp = -1I
        //let mutable temp = -1
        while temp <> c do
           
            temp <- List.reduce (fun acc elem -> 
                                    _power <- _power + 1
                                    acc + elem * BigInteger.Pow( bigint(_x) , _power)) lst
            
            _x <- _x + 1
            _power <- 0
            temp <- temp % p
            
        bf_X <- bigint(_x)
        //for item in 0 .. lst.Length do
        //    let temp = (List.reduce (fun acc elem -> 
        //                                bf_iter <- bf_iter + 1
        //                                acc + elem * BigInteger.Pow( bigint(item) , bf_iter)) lst) % p
        //    if c = temp 
        //       then bf_X <- bigint(item)
            
        bf_X 
   


/// ===========================================
/// White Box Algorithm Part1: POLYNOM GENERATION and CHINESS ALGORITHM
/// ===========================================
module WB_Alghorithm = 

    open Functions

    type Polynom() =

        ///<summary>
        /// List of primes numbers. Private property
        ///</summary>
        member val listPrimeNumbers : bigint list = [] with get, set

        ///<summary>
        /// List of polynomials for each prime number. Private property
        ///</summary>
        member val listPolynoms : bigint list list = [] with get, set

        ///<summary>
        ///The long-awaited biggest polynomial, as one of parameters of public key. Security: public
        ///</summary>
        [<DefaultValue>] val mutable bigPolynoms : bigint list

        
        ///<summary>
        /// Property for polynomial and save it in 'listPolynoms'
        ///</summary>
        /// <param name="p"> prime number </param>
        /// <param name="counter"> test counter </param>
        member this.CreatePolynomFunc p counter =
            
            let mutable _iter = 0
            this.listPrimeNumbers <- p :: this.listPrimeNumbers
            /// <summary>
            /// Residue ring over field of p
            /// </summary>
            let Z : bigint list  = [0I .. p-1I]

            /// <summary>
            /// Residue ring over field of (p-1)
            /// </summary>
            let Zphi : bigint list = DefineResidue_Zphi p     
                
            /// <summary>
            /// Define the count of elementary polynoms (I dvided on 3 intentionally)
            /// </summary>
            let k = Zphi.[rand.Next(0, (Zphi.Length-1))]            
           
            /// <summary>
            /// List of all polynomials [f_k,f_k-1_, ... , f_1]  
            /// </summary>
            let mutable list_of_all_f : bigint list list = [] 
        
            // The Simple Example ///
            // There are some manual polynomials for each primary number  //
            //if counter= 0 
            //    then 
            //        list_of_all_f <- [[1I;0I;3I;0I];[2I;0I;1I;0I];[1I;0I;1I;1I]]
            //if counter= 1 
            //    then 
            //        list_of_all_f <- [[1I;0I;3I;2I];[3I;0I;1I;0I];[1I;0I;1I;1I]]
            //if counter= 2
            //    then 
            //        list_of_all_f <- [[1I;0I;5I;0I];[1I;0I;1I;4I];[2I;0I;1I;1I]]

            list_of_all_f <- Func_List_f k Z Zphi
                                                                   
            // POLYNOMIAL EXPANSION 
            // F_k : 
        
            let a = list_of_all_f.[0].[0]
            let b = list_of_all_f.[0].[1]
            let power = list_of_all_f.[0].[2]                                                
            let d = list_of_all_f.[0].[3]

            /// <summary>
            /// Prepared the list of F_k' coefficients  
            /// </summary>
            let list_Fk_coeff = Func_Fk_coeff p a b power d                
    
            // THE REST 'F' 

            /// <summary>
            /// The mutubale list of coefficents, which is a result of the calculation of previous polynomial, in the future it is big F-polynomial
            /// </summary>
            let mutable f_previous = list_Fk_coeff

            // Loop for define big Polynomial
            for ind = 1 to list_of_all_f.Length-1 do
            
                let _a_i = list_of_all_f.[ind].[0]
                let _b_i = list_of_all_f.[ind].[1]
                let _power = list_of_all_f.[ind].[2]
                let _d_i = list_of_all_f.[ind].[3]

                /// <summary>
                /// The mutubale list of coefficents, which is a result of f(f(x))
                /// </summary>
                let mutable list_f_previous : bigint array = List.toArray f_previous

                if _b_i <> 0I then
                    // Consider a coefficients
                    list_f_previous <- Array.map ((*) _a_i) list_f_previous

                    list_f_previous.[list_f_previous.Length - 1] <- list_f_previous.[list_f_previous.Length - 1] + _b_i 
                
                // Save the result of list_f_previous as a parameter
                let list_param2 = Array.toList list_f_previous
                   
                // Loop for raise a power of small polynomial
                for tt in 1I .. _power-1I do
                    let mutable list_param1 = Array.toList list_f_previous 
                    let (Result : bigint[]) = 
                        Array.zeroCreate (list_param1.Length + list_param2.Length - 1)      // Initiliaze array [|0; 0;...|, it is needed as parameter]
                
                    let func_MultipleLists_Arr = MultipleLists_Arr list_param1 list_param2
                    list_f_previous <- FuncInitResList (func_MultipleLists_Arr, Result)   
                
                if _b_i = 0I then
                     list_f_previous <- Array.map ((*) _a_i) list_f_previous
                
                list_f_previous.[list_f_previous.Length - 1] <- (list_f_previous.[list_f_previous.Length - 1] + _d_i)    
                 
                f_previous <- 
                        Array.toList list_f_previous |> List.map (fun x -> x % p)                                
                
            this.listPolynoms <- f_previous :: this.listPolynoms



        ///<summary>
        /// Property for Chiness Algorithm
        ///</summary>
        /// <param name="p"> list of primes number </param>
        member this.ChinessAlgorithm (p_list : bigint seq) =
    
            let listFpolynomials = this.listPolynoms

            /// <summary>
            /// The count of coefficients of the Big Polynomial. Look at module 'WB_Alghorithm_Part1'
            /// </summary>
            let length_each_f = List.map (fun (lst : bigint list) -> lst.Length) listFpolynomials

            let max_count = List.max length_each_f

            /// Convert sequence of primes number into list
            let P_list = this.listPrimeNumbers
            
            /// <summary>
            /// Multiplying of all modules
            /// </summary>
            let M =  List.reduce (fun acc elem -> acc * elem) <| P_list           
       
            /// Counter
            let mutable _iter = -1

            /// Consider zero coefficients
            let add_zero_rev = List.map (fun (lst : List<bigint>) -> 
                                    let zero_lst = [ for i in 1 .. max_count - lst.Length -> 0I]
                                    List.rev (zero_lst @ lst) 
                                    ) this.listPolynoms 
    
            /// <summary>
            /// Reverse coefficients modulo M_i
            /// </summary>
            let reverse_M_i = List.map (fun i -> 
                                                let res_gcd_ex = Gcd_ex (M/i) (i)
                                                Check_negative (res_gcd_ex) (i)
                                        ) P_list
            
            ///// <summary>
            ///// List of polynomial's transform coefficents to reverse coefficients modulo M_i
            ///// </summary>
            //let lst_poly_rev_m = List.map (fun (lst : bigint list) -> _iter <- _iter + 1 
            //                                                          let res_gcd_ex = Gcd_ex (M/P_list.[_iter]) (P_list.[_iter])
            //                                                          let res_check_negative = Check_negative (res_gcd_ex) (P_list.[_iter])
            //                                                          List.map (fun elem -> 
            //                                                               (elem * (res_check_negative)) % (P_list.[_iter])) lst) add_zero_rev
            
            _iter <- -1
            let como = List.map (fun (lst : bigint list) -> 
                                       _iter <- _iter + 1
                                       lst.[0] * (M/P_list.[_iter]) * reverse_M_i.[_iter]) add_zero_rev
                                                 
       
            this.bigPolynoms <- [(List.sum como) % M]
            
            
            for item in 1 .. max_count - 1 do
                _iter <- -1
                let como = List.map (fun (lst : bigint list) -> 
                                            _iter <- _iter + 1
                                            lst.[item] * (M/P_list.[_iter]) * reverse_M_i.[_iter]) add_zero_rev
                                                 
       
                this.bigPolynoms <- (List.sum como) % M :: this.bigPolynoms
    
            //this.bigPolynoms <- List.rev this.bigPolynoms 
        
        

         /// The end of the  Encryption

 
/// ===========================================
/// White Box: ENCRYPTED MESSAGE
/// ===========================================
module Encryption = 

    open System.Numerics

    type Message() =
        
        [<DefaultValue>] val mutable F_m : bigint


        member this.Encrypted (message : int) (M : bigint) (bigPoly : bigint list) =
            
            // counter
            let mutable _iter = 0  

            this.F_m <- (List.reduce (fun acc elem  ->
                                       _iter <- _iter + 1
                                       acc +  elem * BigInteger.Pow( BigInteger(message), _iter)) bigPoly) % M
           
                                       
              

/// ===========================================
/// White Box Algorithm Part3: DECRYPTED MESSAGE
/// ===========================================
module Decryption = 

    open System.Numerics
    open Functions

    type Message() =
        
        [<DefaultValue>] val mutable message : bigint

        member this.Decrypted (encrptMessage : int) 
            (listPrimesNum : bigint list) (listPolynoms : bigint list list) =
            
            let listP = List.rev listPrimesNum
                     // F(message) = c_i mod p_i and we get C = {...}
            let list_C = List.map (fun p -> bigint(encrptMessage) % p) listP

            
            // The decrypted message
            let mutable X : bigint list = []
            for item in 0 .. listP.Length-1 do
                
                let lstPoly = List.rev listPolynoms.[item]
                let prime = listP.[item]
                let c = list_C.[item]
                X <- Brute_Force lstPoly prime c :: X
            
            X <- List.rev X

            /// <summary>
            /// Multiplying of all modules
            /// </summary>
            let M =  List.reduce (fun acc elem -> acc * elem) <| listP
                

                /// Counter
            let mutable _iter = -1
            let lst_poly_rev_m = List.map (fun elem -> _iter <- _iter + 1 
                                                       let res_gcd_ex = Gcd_ex (M/listP.[_iter]) (listP.[_iter])
                                                       let res_check_negative = Check_negative (res_gcd_ex) (listP.[_iter])
                                                       elem * (res_check_negative) * (M/listP.[_iter])) X
           
            let mutable result = List.sum lst_poly_rev_m % M - 1I
            
            
            while result > 122I do
                result <- result - M
                
            
            while result < 32I do
                result <- result + M

            this.message <- result
          
