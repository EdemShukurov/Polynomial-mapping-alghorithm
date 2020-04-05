using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Collections.Generic;
using System.IO;
using System.Numerics;

namespace Miller_RabinTestLib.Tests
{
    [TestClass()]
    public class MillerRabinTestTests
    {
        private readonly List<BigInteger> _primeNumbers;
        private readonly List<BigInteger> _complexNumbers;

        private readonly MillerRabinTest _millerRabinTest = new MillerRabinTest();

        public MillerRabinTestTests()
        {
            _primeNumbers = new List<BigInteger>();

            string line;

            // Read the file 
            StreamReader file = new StreamReader(@"..\..\Resources\primes.txt");
            while ((line = file.ReadLine()) != null)
            {

                string[] parts = line.Split(new[] { '\t' });

                foreach (var str in parts)
                {
                    string noSpace = str.Trim();
                    if (string.IsNullOrEmpty(noSpace))
                        _primeNumbers.Add(BigInteger.Parse(noSpace));
                }
            }

            file.Close();

            _complexNumbers = new List<BigInteger>();
            for (BigInteger i = 1; i < _primeNumbers.Count; i++)
            {
                if (!_primeNumbers.Contains(i)) _complexNumbers.Add(i);
            }
        }
       


        [TestMethod]
        public void GetPrimeNum_ReturnsTrue()
        {
            bool result;

            for (int i = 0; i < _primeNumbers.Count; i++)
            {
                result = _millerRabinTest.IsPrimeNumber(_primeNumbers[i]);
                Assert.AreEqual(true, result);
            }

        }

        [TestMethod]
        public void GetComplexNum_ReturnsFalse()
        {
            bool result;

            for (int i = 0; i < _complexNumbers.Count; i++)
            {
                result = _millerRabinTest.IsPrimeNumber(_complexNumbers[i]);
                Assert.AreEqual(false, result);
            }

        }

        [TestMethod]
        public void GetPrimeNum_ViaOnePrompt_ReturnsTrue()
        {
            bool result;

            for (int i = 0; i < _primeNumbers.Count; i++)
            {
                result = _millerRabinTest.IsPrimeNumber(_primeNumbers[i], 1);
                Assert.AreEqual(true, result);
            }

        }

        [TestMethod]
        public void GetComplexNum_ViaOnePrompt_ReturnsFalse()
        {
            bool result;

            for (int i = 0; i < _complexNumbers.Count; i++)
            {
                result = _millerRabinTest.IsPrimeNumber(_complexNumbers[i], 1);
                Assert.AreEqual(false, result);
            }

        }
    }
}