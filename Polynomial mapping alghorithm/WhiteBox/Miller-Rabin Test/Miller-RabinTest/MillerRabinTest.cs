using System.Numerics;
using System.Security.Cryptography;

namespace Miller_RabinTestLib
{
    public class MillerRabinTest
    {
        public bool IsPrimeNumber(BigInteger n, byte k = 4)
        {

            /// If n = 2 or 3 then n - primary number and return TRUE, else FALSE
            if (n == 2 || n == 3)
                return true;

            /// If n less 2 or even then n - complex number and return FALSE, else TRUE
            if (n < 2 || n % 2 == 0)
                return false;

            /// Let n − 1 = (2^s)·t, where t is odd, we can do it by sequetial division n-1 by 2
            BigInteger t = n - 1;

            int s = 0;

            while (t % 2 == 0)
            {
                t /= 2;
                s += 1;
            }

            /// Repeat k - times 
            for (int i = 0; i<k; i++)
            {
                /// choose random integer number [2, n − 2]
                RNGCryptoServiceProvider rng = new RNGCryptoServiceProvider();

                byte[] _a = new byte[n.ToByteArray().LongLength];

                BigInteger a;

                do
                {
                    rng.GetBytes(_a);
                    a = new BigInteger(_a);
                }
                while (a< 2 || a >= n - 2);

                /// x ← a^t mod n, conculate by exponentiation modulo
                BigInteger x = BigInteger.ModPow(a, t, n);

                /// if x = 1 or n − 1, then go to the next iteration
                if (x == 1 || x == n - 1)
                    continue;

                /// Repeat s − 1 times
                for (int r = 1; r<s; r++)
                {
                    /// x ← x^2 mod n
                    x = BigInteger.ModPow(x, 2, n);

                    /// if x = 1, then return FALSE
                    if (x == 1)
                        return false;

                    /// If x = n − 1, then go out from this loop
                    if (x == n - 1)
                        break;
                }

                if (x != n - 1)
                    return false;
            }
                        
            /// Return TRUE (possible primary key)
            return true;
        }

         
    }
}
