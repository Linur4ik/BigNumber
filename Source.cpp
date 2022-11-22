#include <iostream>
#include <ctime>
#include <stdbool.h>
#include <string.h>
#include <type_traits> 
#include <string>
#include <bitset>
#include <cmath>
#include <chrono>
#include<vector>

using std::chrono::high_resolution_clock;
using std::chrono::duration_cast;
using std::chrono::duration;
using std::chrono::milliseconds;


#define BaseSize (sizeof(Base)*8)
#define BaseMax ((unsigned long long)1<<BaseSize)
using namespace std;
typedef unsigned char Base;
typedef unsigned int ext;
typedef conditional < BaseSize < 32, conditional<BaseSize<16, unsigned short, unsigned int >::type, unsigned long long>::type tmp;


class BigNum
{
public:
	int MaxLen; // Кол-во выделенной памяти
	int Len; // Кол-во эл-ов массива
	Base* Ptr;

	BigNum(int cur = 1, int flag = 0)
	{
		MaxLen = cur;
		Ptr = new Base[MaxLen];
		if (flag == 0)
		{
			Len = 1;
			for (int i = 0; i < MaxLen; i++) Ptr[i] = 0;
		}
		else if (flag == 1)
		{
			Len = cur;
			unsigned char a;
			unsigned char num;
			for (int i = 0; i < MaxLen; i++)
			{
				Ptr[i] = 0;
				for (int j = 0; j < BaseSize / 8; j++)
				{
					Ptr[i] <<= 8;
					num = rand() % 256;
					Ptr[i] |= num;
				}
			}
			this->Norm();
		}

	}

	BigNum(const BigNum& c)
	{
		if (&c != this)
		{
			MaxLen = c.MaxLen;
			Ptr = new Base[MaxLen];
			Len = c.Len;
			for (int i = 0; i < MaxLen; i++)	Ptr[i] = c.Ptr[i];
		}
	}
	~BigNum()
	{
		if (Ptr != NULL)	delete[] Ptr;
	}
	// 4 sem
	BigNum& operator = (const BigNum&);
	BigNum& operator = (const Base);
	void Norm();
	void Printf16();
	void Scanf16(const char*);
	void Printf10();//-
	void Scanf10(const char*);//-
	bool operator == (const BigNum&);
	bool operator != (const BigNum&);
	bool operator > (const BigNum&);
	bool operator < (const BigNum&);
	bool operator >= (const BigNum&);
	bool operator <= (const BigNum&);
	BigNum operator + (const BigNum&);
	BigNum& operator += (const BigNum&);
	BigNum operator - (const BigNum&);
	BigNum& operator -= (const BigNum&);
	BigNum operator * (const BigNum&);
	BigNum operator+ (const Base);
	BigNum& operator+= (const Base);
	BigNum& operator*= (const Base);
	BigNum& operator*= (const BigNum&);
	BigNum operator * (const Base);
	BigNum operator /(const Base);
	BigNum& operator /=(const Base);
	BigNum operator /(const BigNum&);
	BigNum& operator /=(const BigNum&);
	Base operator %(const Base);
	BigNum operator % (const BigNum&);
	BigNum gcd(const BigNum&);
	void PrintCoef();
	int SubLN(const BigNum&, int);
	void AddLN(const BigNum&, int);
	void AddMaxLen(int);
	int GetLen();
	// 6 sem
	BigNum Fast_Sq();
	BigNum pow(BigNum&);
	BigNum ShiftR(int i);
	BigNum BarretMod(BigNum&, BigNum&);
	Base bits();
	bool bit(Base);
	BigNum stupid_pow(BigNum&);
	bool is_even();
	bool is_odd();
	bool Ferma(int);
	BigNum PowMod(BigNum&, BigNum&);
	bool Solovay_Strassen(int);
	BigNum SqrStr(BigNum, BigNum);
	BigNum GeneratePrime(int);
	bool MillerRab(int);
	float eps_ferma(int);
	BigNum sqrt();
	BigNum phi();
	// 7 sem
	vector<BigNum> fact();
	BigNum algorithmAlway(BigNum);
	pair<BigNum, BigNum>methodFermat();
	BigNum FunPolard(BigNum);
	BigNum Polard();
	BigNum MinusMod(BigNum);
	BigNum Dixon(int);
	vector<BigNum> pGlad(vector <BigNum>, BigNum);
	BigNum Gelfond(BigNum, BigNum);
	BigNum pPolard(BigNum, BigNum);
	vector<BigNum> Fx(vector<BigNum>, BigNum, BigNum);
	BigNum ph(BigNum);
};

BigNum& BigNum :: operator = (BigNum const& c)
{
	if (&c != this)
	{
		MaxLen = c.MaxLen;
		Len = c.Len;
		delete[]Ptr;
		Ptr = new Base[MaxLen];
		for (int i = 0; i < MaxLen; i++) Ptr[i] = c.Ptr[i];
		return *this;
	}
	return *this;
}

BigNum& BigNum :: operator = (const Base num)
{
	delete[]Ptr;
	Len = 1;
	MaxLen = Len;
	Ptr = new Base[Len];
	Ptr[0] = num;
	return *this;
}

void BigNum::Norm()
{
	for (Len = MaxLen; Ptr[Len - 1] == 0 && Len > 0; Len--);
	if (Len == 0) Len = 1;
}

BigNum BigNum::stupid_pow(BigNum& n)
{
	BigNum i; i = (Base)0;
	BigNum res; res = (Base)1;
	while (i < n)
	{
		res *= *this;
		i += 1;
	}
	return res;
}

int BigNum::GetLen()
{
	return Len;
}

void BigNum::PrintCoef()
{
	for (int i = MaxLen - 1; i >= 0; i--) cout << bitset<8>(Ptr[i]) << " ";
	cout << endl;
}

void BigNum::Printf16()
{
	bool k = true;
	for (int i = Len - 1; i >= 0; i--)
	{
		for (int j = BaseSize - 4; j >= 0; j = j - 4)
		{
			char Tmp = ((Ptr[i] & (0xF << j)) >> j);
			if (k && (Tmp == 0)) continue;
			else k = false;
			if (Tmp < 10) {
				Tmp = Tmp + '0';
				cout << Tmp;
			}
			else if (Tmp < 16) {
				Tmp = Tmp + 'a' - 10;
				cout << Tmp;
			}
		}
	}
	if (k) cout << 0;
}

void BigNum::Scanf16(const char* str)
{
	Len = ((strlen(str) - 1) / (BaseSize / 4)) + 1;
	int k = Len - 1, j = 0;

	delete[] Ptr; // Удаляем память
	MaxLen = Len;
	Ptr = new Base[Len];

	for (int i = 0; i < Len; i++)
	{
		Ptr[i] = 0;
	}

	if (strlen(str) % (BaseSize / 4) > 0)
	{
		for (j = 0; j < strlen(str) % (BaseSize / 4) - 1; j++)
		{
			if ((int)str[j] >= 48 && (int)str[j] <= 57)     // записываем str[i] - 48
			{
				Ptr[k] = (Base)((int)str[j] - 48) | Ptr[k];
				Ptr[k] = Ptr[k] << 4;
			}
			else if ((int)str[j] >= 97 && (int)str[j] <= 102) // записываем str[i] - 87
			{
				Ptr[k] = (Base)((int)str[j] - 87) | Ptr[k];
				Ptr[k] = Ptr[k] << 4;
			}
			else Ptr[k] = 0;
		}
		if ((int)str[j] >= 48 && (int)str[j] <= 57)     // записываем str[i] - 48
		{
			Ptr[k] = (Base)((int)str[j] - 48) | Ptr[k];
		}
		else if ((int)str[j] >= 97 && (int)str[j] <= 102) // записываем str[i] - 87
		{
			Ptr[k] = (Base)((int)str[j] - 87) | Ptr[k];
		}
		else Ptr[k] = 0;
		k--;
		j++;
	}
	while (k >= 0)
	{
		for (; (j - (strlen(str) % (BaseSize / 4))) % (BaseSize / 4) < BaseSize / 4 - 1; j++)
		{
			if ((int)str[j] >= 48 && (int)str[j] <= 57)     // записываем str[i] - 48
			{
				Ptr[k] = (Base)((int)str[j] - 48) | Ptr[k];
				Ptr[k] = Ptr[k] << 4;
			}
			else if ((int)str[j] >= 97 && (int)str[j] <= 102) // записываем str[i] - 87
			{
				Ptr[k] = (Base)((int)str[j] - 87) | Ptr[k];
				Ptr[k] = Ptr[k] << 4;
			}
			else Ptr[k] = 0;
		}
		if ((int)str[j] >= 48 && (int)str[j] <= 57)     // записываем str[i] - 48
		{
			Ptr[k] = (Base)((int)str[j] - 48) | Ptr[k];
		}
		else if ((int)str[j] >= 97 && (int)str[j] <= 102) // записываем str[i] - 87
		{
			Ptr[k] = (Base)((int)str[j] - 87) | Ptr[k];
		}
		else Ptr[k] = 0;
		k--; j++;
	}
	Norm();
}

bool BigNum::operator==(const BigNum& bn)
{
	if (Len != bn.Len) {
		return false;
	}

	for (size_t i = 0; i < Len; i++) {
		if (Ptr[i] != bn.Ptr[i]) {
			return false;
		}
	}

	return true;
}

bool BigNum :: operator != (const BigNum& a)
{
	if (&a == this) return 0;
	if (Len != a.Len) return 1;
	int i = Len - 1;
	while (i >= 0)
	{
		if (Ptr[i] == a.Ptr[i]) i--;
		else return true;
	}
	return false;
}

bool BigNum :: operator > (const BigNum& a)
{
	if (&a == this) return false;
	if (Len > a.Len) return true;
	if (Len < a.Len) return false;
	int i = Len - 1;
	while (i >= 0)
	{
		if (Ptr[i] == a.Ptr[i]) i--;
		else if (Ptr[i] > a.Ptr[i]) return true;
		else return false;
	}
	return false;
}

bool BigNum :: operator < (const BigNum& a)
{
	if (&a == this) return false;
	if (Len < a.Len) return true;
	if (Len > a.Len) return false;
	int i = Len - 1;
	while (i >= 0) {
		if (Ptr[i] == a.Ptr[i]) i--;
		else if (Ptr[i] < a.Ptr[i]) return true;
		else return false;
	}
	return false;
}

bool BigNum :: operator >= (const BigNum& a)
{
	if (Len > a.Len) return true;
	if (Len < a.Len) return false;
	for (int i = Len - 1; i >= 0; i--)
	{
		if (Ptr[i] < a.Ptr[i]) return false;
		if (Ptr[i] > a.Ptr[i]) return true;
	}
	return true;
}

bool BigNum :: operator <= (const BigNum& a)
{
	if (Len < a.Len) return true;
	if (Len > a.Len) return false;
	for (int i = Len - 1; i >= 0; i--)
	{
		if (Ptr[i] > a.Ptr[i]) return false;
		if (Ptr[i] < a.Ptr[i]) return true;
	}
	return true;
}

BigNum BigNum::operator+ (const BigNum& a)
{
	int min, max;
	Base* MaxCoef;
	bool k = 0;
	tmp TMP;
	if (Len > a.Len)
	{
		min = a.Len;
		max = Len + 1;
		MaxCoef = Ptr;
	}
	else
	{
		min = Len;
		max = a.Len + 1;
		MaxCoef = a.Ptr;
	}
	BigNum A(max, 0);
	for (int i = 0; i < min; i++)
	{
		TMP = (tmp)a.Ptr[i] + (tmp)Ptr[i] + k;
		A.Ptr[i] = (Base)TMP;
		k = (bool)(TMP >> BaseSize);
	}
	for (int i = min; i < max - 1; i++)
	{
		TMP = (tmp)MaxCoef[i] + k;
		A.Ptr[i] = (Base)TMP;
		k = (bool)(TMP >> BaseSize);
	}
	A.Ptr[max - 1] = k;
	A.Norm();
	return A;
}

BigNum& BigNum ::operator += (const BigNum& a)
{
	*this = *this + a;
	return *this;
}

BigNum BigNum::operator - (const BigNum& a)
{
	if (*this < a)
	{
		return *this;
	}
	tmp TMP;
	bool k = 0;
	BigNum A(*this);
	for (int i = 0; i < a.Len; i++)
	{
		TMP = A.Ptr[i] + BaseMax - a.Ptr[i] - k;
		A.Ptr[i] = (Base)TMP;
		k = !((TMP) >> BaseSize);
	}
	for (int i = a.Len; k && i <= A.Len; i++)
	{
		TMP = A.Ptr[i] + BaseMax - k;
		A.Ptr[i] = (Base)TMP;
		k = !((TMP) >> BaseSize);
	}
	A.Norm();
	return A;
}

BigNum& BigNum::operator -= (const BigNum& LN)
{
	if (*this < LN)
	{
		cout << "first value should be bigger than second to subtract" << endl;
		return *this;
	}
	int min;
	bool k = 0;
	tmp TMP;
	for (int i = 0; i < LN.Len; i++)
	{
		TMP = Ptr[i] + BaseMax - LN.Ptr[i] - k;
		k = !((TMP) >> BaseSize);
		Ptr[i] = (Base)TMP;
	}
	for (int i = LN.Len; k && i <= Len; i++)
	{
		TMP = Ptr[i] + BaseMax - k;
		k = !((TMP) >> BaseSize);
		Ptr[i] = (Base)TMP;
	}
	this->Norm();
	return *this;
}

BigNum BigNum::operator+ (const Base a)
{
	tmp Tmp = 0;
	bool k = 0;
	int len = MaxLen;
	if (Len + 1 > MaxLen) len = Len + 1;
	BigNum A(len, 0);
	Tmp = (tmp)Ptr[0] + (tmp)a + k;
	k = (bool)(Tmp >> BaseSize);
	A.Ptr[0] = Tmp;
	for (int i = 1; i < Len; i++)
	{
		Tmp = Ptr[i] + k;
		k = bool(Tmp >> BaseSize);
		A.Ptr[i] = Tmp;
	}
	A.Ptr[Len] = k;
	A.Norm();
	return A;
}

BigNum& BigNum::operator+= (const Base a)
{
	*this = *this + a;
	return *this;
}

BigNum BigNum :: operator*(const Base a)
{
	tmp Tmp = 0;
	Base k = 0;
	BigNum A(Len + 1, 0);
	for (int i = 0; i < Len; i++)
	{
		Tmp = (tmp)Ptr[i] * (tmp)a + k;
		k = (Tmp >> BaseSize);
		A.Ptr[i] = (Base)Tmp;
	}
	A.Ptr[Len] = k;
	A.Norm();
	return A;
}

BigNum BigNum ::operator * (const BigNum& a)
{
	tmp Tmp;
	Base k;
	BigNum A(Len + a.Len, 0);
	for (int i = 0; i < Len; i++)
	{
		k = 0;
		for (int j = 0; j < a.Len; j++)
		{
			Tmp = (tmp)a.Ptr[j] * (tmp)Ptr[i] + A.Ptr[i + j] + k;
			k = (Tmp >> BaseSize);
			A.Ptr[i + j] = Tmp;
		}
		A.Ptr[a.Len + i] += k;
	}
	A.Norm();
	return A;
}

BigNum& BigNum::operator*= (const Base a)
{
	*this = *this * a;
	return *this;
}

BigNum& BigNum::operator *= (const BigNum& a)
{
	*this = *this * a;
	return *this;
}

Base BigNum :: operator %(const Base a)
{
	tmp Tmp, k = 0;
	for (int i = Len - 1; i >= 0; i--)
	{
		Tmp = (k << BaseSize) + Ptr[i];
		k = Tmp % a;
	}
	return k;
}

BigNum BigNum::operator/(Base num)
{
	tmp TMP, k = 0;
	BigNum A(Len, 0);
	for (int i = Len - 1; i >= 0; i--)
	{
		TMP = (k << BaseSize) + Ptr[i];
		k = TMP % num;
		A.Ptr[i] = (TMP) / num;
	}
	A.Norm();
	return A;
}

BigNum& BigNum::operator/=(Base num) {
	*this = *this / num;
	return *this;
}

int BigNum::SubLN(const BigNum& a, int j)
{
	tmp TMP;
	bool k = 0;
	for (int i = 0; i < a.Len; i++)
	{
		TMP = (tmp)Ptr[i + j] + BaseMax - a.Ptr[i] - k;
		Ptr[i + j] = (Base)TMP;
		k = !((TMP) >> BaseSize);
	}
	for (int i = a.Len; k && (i + j) < Len; i++)
	{
		TMP = (tmp)Ptr[i + j] + BaseMax - k;
		Ptr[i + j] = (Base)TMP;
		k = !((TMP) >> BaseSize);
	}
	Norm();
	return k;
}

BigNum BigNum::gcd(const BigNum& bn)
{
	BigNum a = *this;
	BigNum b = bn;

	while (a != b)
	{
		if (a > b)
			a = a - b;
		else b = b - a;
	}
	return a;
}

void BigNum::AddLN(const BigNum& a, int j)
{
	bool k = 0;
	for (int i = 0; i < a.Len; i++)
	{
		tmp TMP = (tmp)Ptr[i + j] + a.Ptr[i] + k;
		k = bool(TMP >> BaseSize);
		Ptr[i + j] = TMP;
	}
	for (int i = a.Len; k && (i + j) < Len; i++)
	{
		tmp TMP = (tmp)Ptr[i + j] + k;
		k = bool(TMP >> BaseSize);
		Ptr[i + j] = TMP;
	}
	//NormLen();
}

void BigNum::AddMaxLen(int b)
{
	MaxLen += b;
	Base* NewCoef = new Base[MaxLen];
	for (int i = 0; i < Len; i++) NewCoef[i] = Ptr[i];
	for (int i = Len; i < MaxLen; i++) NewCoef[i] = 0;
	delete[]Ptr;
	Ptr = NewCoef;
}

BigNum BigNum::operator%(const BigNum& a)
{
	if ((a.Len == 1) && (a.Ptr[0] != 0))
	{
		BigNum r(1, 0);
		r.Ptr[0] = *this % a.Ptr[0];
		return r;
	}
	if ((a.Len == 1) && (a.Ptr[0] == 0))
	{
		BigNum r(*this);
		return r;
	}
	if (*this == a)
	{
		BigNum r(1, 0);
		return r;
	}
	if (*this < a)
	{
		//cout << "!" << endl;
		return *this;
	}
	BigNum q(a.Len - 1);
	tmp b = BaseMax;
	//D1
	tmp d = b / ((tmp)a.Ptr[a.Len - 1] + 1); // приближенное значение
	BigNum dU(*this); // dU/dV
	BigNum dV(a);
	// Увеличиваем число на разряд
	dU = dU * d; // Умножение U and V на d
	dV = dV * d;
	if (Len == dU.Len)
	{
		if (dU.Len == dU.MaxLen)
		{
			dU.AddMaxLen(1);
		}
		dU.Ptr[Len] = 0;
		dU.Len = Len + 1;
	}
	//D2
	for (int j = Len - a.Len; j >= 0; j--)
	{
		//D7(Цикл по j)
			//D3 Вычисляем прибл.делимое
		int k = j + a.Len;
		tmp ff = ((tmp)dU.Ptr[k] << BaseSize) + dU.Ptr[k - 1];
		tmp Pq = (ff / dV.Ptr[a.Len - 1]);
		tmp Pr = (ff % dV.Ptr[a.Len - 1]);
		if ((Pq == b) || (Pq * dV.Ptr[a.Len - 2]) > (b * Pr + dU.Ptr[k - 2]))
		{
			Pq--;
			Pr = Pr + dV.Ptr[a.Len - 1];
			// исключаем когда прибл. значени на единицу больше истиного и когда q больше на 2 
			if ((Pr < b) && ((Pq == b) || (Pq * dV.Ptr[a.Len - 2] > b * Pr + dU.Ptr[k - 2])))
			{
				Pq--;
				Pr = Pr + dV.Ptr[a.Len - 1];
			}
		}
		//D5	
		//D4 деление столбиком
		int s = dU.SubLN(dV * Pq, j);
		if (s) // проверка остатка D5
		{
			//D6 компенсирование сложения
			//q.Ptr[j]--;
			dU.AddLN(dV, j); // компенсация займа
		}
	}
	//D8
	dU.Len = a.Len;
	q = dU / d;
	return q;
}

BigNum BigNum::operator/(const BigNum& a)
{
	if ((a.Len == 1) && (a.Ptr[0] != 0))
	{
		BigNum q = *this / a.Ptr[0];
		return q;
	}
	if ((a.Len == 1) && (a.Ptr[0] == 0))
	{
		BigNum q(1, 0);
		q.Ptr[0] = 0;
		return q;
	}
	if (*this == a)
	{
		BigNum q(1, 0);
		q.Ptr[0] = 1;
		return q;
	}
	if (*this < a)
	{
		BigNum q;
		return q;
	}
	BigNum q(Len - a.Len + 1);
	tmp b = BaseMax;
	//D1
	tmp d = b / ((tmp)a.Ptr[a.Len - 1] + 1); // приближенное значение
	BigNum dU(*this); // dU/dV
	BigNum dV(a);
	// Увеличиваем число на разряд
	dU = dU * d; // Умножение U and V на d
	dV = dV * d;
	if (Len == dU.Len)
	{
		if (dU.Len == dU.MaxLen)
		{
			dU.AddMaxLen(1);
		}
		dU.Ptr[Len] = 0;
		dU.Len = Len + 1;
	}
	//D2
	for (int j = Len - a.Len; j >= 0; j--)
	{
		//D7(Цикл по j)
			//D3 Вычисляем прибл.делимое
		int k = j + a.Len;
		tmp ff = ((tmp)dU.Ptr[k] << BaseSize) + dU.Ptr[k - 1];
		tmp Pq = (ff / dV.Ptr[a.Len - 1]);
		tmp Pr = (ff % dV.Ptr[a.Len - 1]);
		if ((Pq == b) || (Pq * dV.Ptr[a.Len - 2]) > (b * Pr + dU.Ptr[k - 2]))
		{
			Pq--;
			Pr = Pr + dV.Ptr[a.Len - 1];
			// исключаем когда прибл. значени на единицу больше истиного и когда q больше на 2 
			if ((Pr < b) && ((Pq == b) || (Pq * dV.Ptr[a.Len - 2] > b * Pr + dU.Ptr[k - 2])))
			{
				Pq--;
				Pr = Pr + dV.Ptr[a.Len - 1];
			}
		}
		//D5	
		q.Ptr[j] = Pq;
		//D4 деление столбиком
		int s = dU.SubLN(dV * Pq, j);
		if (s) // проверка остатка D5
		{
			//D6 компенсирование сложения
			q.Ptr[j]--;
			dU.AddLN(dV, j); // компенсация займа
		}
	}
	//D8
	q.Norm();
	return q;
}

void BigNum::Printf10() // Вывод 10чный
{
	if (Len == 1 && Ptr[0] == 0) cout << 0;
	else {
		BigNum A(*this);
		int SLen = Len * 10;
		int k = 0;
		char* S = new char[SLen];
		while (A.Ptr[0] != 0 || A.Len > 1)
		{
			S[k] = (A % 10) + '0';
			A /= 10;
			k++;
		}
		k--;
		for (; k >= 0; k--) cout << S[k];
		delete[]S;
	}
}

void BigNum::Scanf10(const char* a)
{
	tmp TMP = 0;
	Base k = 0;
	int BS = BaseSize / 4;
	int Slen = strlen(a);
	MaxLen = (Slen - 1) / BS + 2;
	Len = 1;
	delete[]Ptr;
	Ptr = new Base[MaxLen];
	for (int i = 0; i < MaxLen; i++) Ptr[i] = 0;
	for (int i = 0; i < Slen; i++)
	{
		char s = 0;
		if ((a[i] >= '0') && (a[i] <= '9')) s = a[i] - '0';
		else continue;
		*this *= 10;
		*this = *this + s;
	}
	Norm();
}

Base BigNum::bits()
{
	for (int i = Len - 1; i >= 0; i--)
	{
		for (int bit = BaseSize - 1; bit >= 0; bit--)
		{
			if ((Ptr[i] >> bit) & 1) return (i * BaseSize + bit + 1);
		}
	}
	return 0;
}

bool BigNum::bit(Base i)
{
	int pos = i / BaseSize;
	return ((Ptr[pos] >> (i % BaseSize)) & 1);
}

BigNum BigNum::Fast_Sq()
{
	BigNum res(2 * Len, 0);
	for (int i = 0; i < Len; i++)
	{
		ext cuv = res.Ptr[2 * i] + ext(Ptr[i]) * Ptr[i];
		res.Ptr[2 * i] = cuv;
		for (int j = i + 1; j < Len; j++)
		{
			cuv = ext(2) * Ptr[i] * Ptr[j] + res.Ptr[i + j] + (cuv >> BaseSize);
			res.Ptr[i + j] = cuv;
		}
		tmp* y = (tmp*)&res.Ptr[i + Len];
		*y += cuv >> BaseSize;
	}
	res.Norm();
	return res;
}

BigNum BigNum::pow(BigNum& n)
{
	BigNum q(*this);
	BigNum z(1, 0);
	if (n.Ptr[0] & 1)
	{
		z = BigNum(*this);
	}
	else
	{
		z += Base(1);
	}
	Base n_bits = n.bits();
	for (int i = 1; i < n_bits; i++)
	{
		q = q.Fast_Sq();
		if (n.bit(i)) z *= q;
	}
	return z;
}

BigNum BigNum::PowMod(BigNum& pow, BigNum& mod)
{
	BigNum null;
	if (pow == null) return Base(1);
	if (mod.Ptr[0] == 1 && mod.Len == 1)
	{
		BigNum res;
		return res;
	}
	int n = (pow.Len - 1) * BaseSize;
	int idx = BaseSize - 1;
	while (true)
	{
		if (pow.Ptr[pow.Len - 1] & (1 << idx))
		{
			n += idx + 1;
			break;
		}
		idx--;
	}
	BigNum z(1);
	z = (*this) % mod;
	for (int i = n - 2; i >= 0; i--)
	{
		z = z.Fast_Sq() % mod;
		if (pow.Ptr[i / BaseSize] & (1 << (i % BaseSize)))
		{
			z = (z * (*this)) % mod;
		}
	}
	return z;
}

BigNum BigNum::ShiftR(int i)
{
	size_t new_size = (i > Len) ? 0 : Len - i;
	BigNum res(new_size, 0);
	res.Len = new_size;
	for (size_t factor = 0; factor < new_size; factor++)	res.Ptr[factor] = Ptr[factor + i];
	return res;
}

BigNum  GetBarretZ(BigNum& m)
{
	BigNum z(2 * m.Len + 1, 0);
	z.Ptr[2 * m.Len] = 1;
	z.Len = 2 * m.Len + 1;
	return z / m;
}

BigNum BigNum::BarretMod(BigNum& m, BigNum& z)
{
	if (Len > 2 * m.Len)
	{
		cout << "Error Len" << endl;
		return *this;
	}
	BigNum q = ShiftR(m.Len - 1); // x / b^(k-1)
	q *= z; // x / b^(k-1) *  z 
	q = q.ShiftR(m.Len + 1); // x / b^(k-1) *  z / b^(k+1)
	BigNum r1 = *this; // x
	BigNum r2 = q * m; // r2=q*m
	BigNum r(m.Len + 2, 0);
	if (r1 >= r2)
	{
		r = (r1 - r2);
	}
	else
	{
		r.Ptr[m.Len + 1] = 1;
		r.Len = m.Len + 2;
		r += r1 - r2; // r = b*(k+1) + r1 - r2
	}
	while (r >= m)
	{
		r -= m;
	}
	return r;
}

void TestMod(int N, int M)
{
	BigNum x(M - M / 3, 1);
	BigNum z = GetBarretZ(x);
	auto start = high_resolution_clock::now();
	for (int i = 0; i < N; i++)
	{
		BigNum f(M, 1);
		BigNum f_mod = f.BarretMod(x, z);
	}
	auto end = high_resolution_clock::now();
	auto ms_int = duration_cast<milliseconds>(end - start);
	cout << "Barret mod : " << ms_int.count() << endl;
	start = high_resolution_clock::now();
	for (int i = 0; i < N; i++)
	{
		BigNum f(M, 1);
		BigNum f_mod = f % x;
	}
	end = high_resolution_clock::now();
	ms_int = duration_cast<milliseconds>(end - start);
	cout << "Standard mod : " << ms_int.count() << endl;
	for (int i = 0; i < N; i++)
	{
		BigNum f(M, 1);
		BigNum f_mod = f % x;
		if (f.BarretMod(x, z) != f_mod) cout << "ALLERT";
	}
}

bool BigNum::Ferma(int t)
{
	BigNum test;
	test.Ptr[0] = 1;
	if (*this == test)
	{
		return true;
	}
	test.Ptr[0] = 2;
	if (*this == test)
	{
		return true;
	}
	test.Ptr[0] = 3;
	if (*this == test)
	{
		return true;
	}

	if (this->is_even())
	{
		return false;
	}
	BigNum number2, n_2;
	number2.Ptr[0] = 2; // number2 = 2
	n_2 = (*this) - number2; // n_2 = n - 2
	BigNum number1, n_1;
	number1.Ptr[0] = 1; // number1 = 2
	n_1 = (*this) - number1; // n_1 = n - 1
	while (t--)
	{
		BigNum a;
		while (true)
		{
			BigNum a_tmp(1 + rand() % Len, 1);
			if (number2 <= a_tmp && a_tmp <= n_2) // 2 <= a <= n-2
			{
				a = a_tmp;
				break;
			}
		}
		BigNum res = a.PowMod(n_1, *this); // r = a^(n-1) mod n
		if (res != number1) // Если res != 1, то составное
		{
			return false;
		}
	}
	return true;
}

bool BigNum::is_even()
{
	return Ptr[0] % 2 == 0;
}

bool BigNum::is_odd()
{
	return Ptr[0] % 2 == 1;
}

BigNum BigNum::GeneratePrime(int len) // Gordon
{
	int T = 111;
	BigNum s; // Генерируем простые s и t одинакового размера
	while (true)
	{
		BigNum a(len, true);
		if (a.MillerRab(T))
		{
			s = a;
			break;
		}
	}
	BigNum t;
	while (true)
	{
		BigNum a(len, true);
		if (a.MillerRab(T))
		{
			t = a;
			break;
		}
	}
	BigNum number2, number1;
	number2.Ptr[0] = 2;
	number1.Ptr[0] = 1;
	BigNum i0(1, true), r; // Берем случайное i0
	while (true)
	{
		r = i0 * t * number2 + number1; // r = 2 * i * t + 1
		if (r.MillerRab(T))
		{
			break;
		}
		i0 = i0 + number1;
	}
	BigNum p0; // p0 = 2*(s^(r-2)mod r) * s - 1
	p0 = r - number2;
	p0 = s.PowMod(p0, r) * s * number2 - number1;
	BigNum j0(1, true), p; // Берем случайное j0
	while (true)
	{
		p = j0 * r * s * number2 + p0; // p = 2 * j * r * s + p0
		if (p.MillerRab(T))
		{
			break;
		}
		j0 = j0 + number1;
	}
	return p;
}

unsigned long long fi(unsigned long long n)
{
	unsigned long long f = n;
	for (unsigned long long i = 2; i * i <= n; i += 1)
	{
		if (n % i == 0)
		{
			while (n % i == 0)
			{
				n /= i;
			}
			f -= f / i;
		}
	}
	if (n > 1)
	{
		f -= f / n;
	}
	return f;
}

float eps_Ferma(BigNum n, int t)
{
	return pow(((fi(n.Ptr[0])) / float(n.Ptr[0])), t);
}

float eps_Miller(BigNum n, int t)
{
	return pow(((fi(n.Ptr[0])) / float((unsigned long long(4) * n.Ptr[0]))), t);
}

void TestPrime(int n, int k, int count)
{
	while (n)
	{
		BigNum rnd(k, true);
		rnd.Printf10();
		auto start = high_resolution_clock::now();
		if (rnd.Ferma(count))
		{
			cout << "\n\nTest Ferma: prime number";
		}
		else
		{
			cout << "\n\nTest Ferma: composite number";
		}
		cout << endl << "eps= " << float(eps_Ferma(rnd, count));
		auto end = high_resolution_clock::now();
		auto ms_int = duration_cast<milliseconds>(end - start);
		cout << " Ferma Time : " << ms_int.count() << endl;
		start = high_resolution_clock::now();
		if (rnd.MillerRab(count))
		{
			cout << "\nMiller - Rabin: prime number";
		}
		else
		{
			cout << "\nMiller  - Rabin: composite number";
		}
		cout << endl << "eps= " << float(eps_Miller(rnd, count));
		end = high_resolution_clock::now();
		ms_int = duration_cast<milliseconds>(end - start);
		cout << " Miller-Rab Time: " << ms_int.count() << endl;
		cout << "\n" << endl;
		n--;
	}
}

bool BigNum::MillerRab(int t)
{
	BigNum test;
	test.Ptr[0] = 1;
	if (*this == test)
	{
		return true;
	}
	test.Ptr[0] = 2;
	if (*this == test)
	{
		return true;
	}
	test.Ptr[0] = 3;
	if (*this == test)
	{
		return true;
	}

	if (this->is_even())
	{
		return false;
	}
	BigNum zero(1, 0), Number_1, r, tmp, n1;
	Base s = 0;
	Number_1.Ptr[0] = 1; // Number_1 = 1
	tmp = (*this) - Number_1;
	n1 = tmp; // n1 =  n - 1
	while (BigNum(tmp % 2) == zero) // 2^s
	{
		tmp = tmp / 2;
		s++;
	}
	r = tmp;
	BigNum Number2, n2; // Number2 = 2; n2 = n - 2
	Number2.Ptr[0] = 2;
	n2 = (*this) - Number2;
	while (t--)
	{
		BigNum b;
		while (true)
		{
			BigNum b_tmp(1 + rand() % Len, true);
			if (Number2 <= b_tmp && b_tmp <= n2) // Выбираем b случайное
			{
				b = b_tmp;
				break; // 2 <= b <= n-2
			}
		}
		BigNum y;
		y = b.PowMod(r, *this); // y = b^r mod n
		if (y != n1 && y != Number_1) // y!=+-1
		{
			Base j = 1;
			while (j < s && y != n1)
			{
				y = y.PowMod(Number2, *this); // y=y^2 mod n
				if (y == Number_1)
				{
					return false;
				}
				j++;
			}
			if (y != n1)
			{
				return false;
			}
		}
	}
	return true; // Число может быть простым
}

void TestGen(int k)
{
	for (size_t i = 0; i < k; i++)
	{
		BigNum gen;
		gen = gen.GeneratePrime(4);
		gen.Printf10();
		if (gen.MillerRab(111))
		{
			cout << "\t\tTest Miller - Rabin: prime number";
		}
		else
		{
			cout << "\t\tTest Miller - Rabin: composite number";
		}
		cout << endl;
		if (gen.Ferma(111))
		{
			cout << "\t\tFerma: prime number";
		}
		else
		{
			cout << "\t\tFerma: composite number";
		}
		cout << endl;
	}
}

BigNum BigNum::sqrt()
{
	BigNum x(*this), x0;
	do
	{
		x0 = x;
		x = (*this / x + x) / 2;
	} while (x < x0);
	return x0;
}

vector<BigNum> BigNum::fact()
{
	vector<BigNum> res;
	if (Ferma(10)) // Проверка на простату  
	{
		res.push_back(*this);
		return res;
	}
	BigNum dk, three, five, seven, dkPrev, dkPrevPrev, one, n(*this), two, zero;
	two += Base(2);
	while (n % Base(2) == Base(0))
	{
		res.push_back(two);
		n /= Base(2);
	}
	one += Base(1);
	zero = one - one;
	three += Base(3);
	five += Base(5);
	seven += Base(7);
	auto ds = sqrt();
	auto    k = 1,
		preK = 0;
	while (dk < ds && n != one)
	{
		if (k != preK) switch (k) // Последовательность пробных делителей 
		{
		case 1:
			dk = three;
			break;
		case 2:
			dkPrev = dk;
			dk = five;
			break;
		case 3:
			dkPrevPrev = dkPrev;
			dkPrev = dk;
			dk = seven;
			break;
		default:
			auto tmp = dkPrev;
			dkPrev = dk;
			dk = dkPrevPrev + Base(6);
			dkPrevPrev = tmp;
			break;
		}
		auto    q = n / dk, // 3
			r = n % dk; // 3
		if (r == zero) // 4
		{
			res.push_back(dk);
			preK = k;
			n = q;
		}
		else if (q > dk) // 5
		{
			preK = k;
			++k;
		}
		else
		{
			res.push_back(n);
			break;
		}
	}
	return res;
}

pair<BigNum, BigNum> BigNum::methodFermat() // Поиск делителей
{
	if (Ferma(10)) { cout << "n is a prime number" << endl; return make_pair(Base(1), *this); }
	if (*this % BigNum(Base(2)) == Base(0)) { cout << "n must be odd" << endl; return make_pair(*this, *this); }
	auto x = sqrt();
	if (x.Fast_Sq() == *this) return make_pair(x, x);
	BigNum  y, z;
	do
	{
		x += Base(1);
		if (x == (*this + Base(1)) / Base(2)) { cout << "n is a prime number" << endl; return make_pair(Base(1), *this); }
		z = x.Fast_Sq() - *this;
		y = z.sqrt();
	} while (y.Fast_Sq() != z);
	return make_pair(x + y, x - y);
}

BigNum BigNum::Polard() // Поиск нетривиального делителя 
{
	BigNum d, One;
	One += Base(1);
	BigNum a, b;
	a += Base(2);
	b += Base(2);
	do
	{
		a = FunPolard(a);
		b = FunPolard(FunPolard(b));
		if (a == b) return *this;
		d = gcd(a.MinusMod(b));
	} while (d == One);
	return d;
}

BigNum BigNum::FunPolard(BigNum x)
{
	BigNum One;
	One += Base(1);
	return (x.Fast_Sq() + One) % *this;
}

BigNum BigNum::MinusMod(BigNum x)
{
	if (*this > x) return *this - x;
	else return x - *this;
}

BigNum BigNum::Dixon(int k) // Не законченно
{
	vector <BigNum> C;
	vector<BigNum> S;
	vector<vector<BigNum>> V;
	BigNum One, I, t, a, b, Two;
	Two += Base(2);
	One += Base(1);
	I += One;
	vector<pair<BigNum, BigNum>> X;
	for (int i = 0; i < k; i++)
	{
		I += One;
		t += One; // t=k
		if (I.MillerRab(10))
		{
			S.push_back(I);
		}
	}
	t += One; // t = k+1
	for (BigNum i; i < t; i += One)
	{
		while (1)
		{
			BigNum a(1, 1);
			BigNum b;
			b = a.Fast_Sq() % *this;
			C = pGlad(S, b);

			for (auto i = 0; i < C.size(); i++)  C[i] = C[i] % Two;
			if (!C.empty())
			{
				X.push_back(make_pair(a, b));
				V.push_back(C);
				break;
			}
		}
	}
	return I;
}

vector<BigNum> BigNum::pGlad(vector <BigNum> S, BigNum x)
{
	vector <BigNum> P = x.fact();
	vector <BigNum> res(S.size());
	BigNum Two;
	Two += Base(2);
	for (auto i = 0; i < P.size(); i++)
	{
		bool str = false;
		for (auto j = 0; j < S.size(); j++)
		{
			if (S[j] == P[i])
			{
				res[j] += Base(1);
				str = true;
				break;
			}
		}
		if (!str) { res.clear(); return res; }
	}
	return res;
}

vector<vector<BigNum>> MatrixUm(vector<vector<BigNum>> a, vector<vector<BigNum>> b)
{
	BigNum A;
	A = a[0][1];

	return a;
}

vector<pair<BigNum, BigNum>> Shall(vector<pair<BigNum, BigNum>> str) // Пузырек шелл
{
	int len = str.size();
	int d = (len) / 2, j;
	pair<BigNum, BigNum> swap;
	for (; d > 0; d = d / 2)
		for (int j = d; j < len; j++)
			for (int i = j - d; i >= 0 && str[i].first > str[i + d].first; i = i - d)
			{
				swap = str[i];
				str[i] = str[i + d];
				str[i + d] = swap;
			}
	return str;
}

pair<bool, BigNum> BitSearch(vector<pair<BigNum, BigNum>> str, BigNum key)
{
	int l = 0;
	int r = str.size();
	bool flag = false;
	while ((l <= r) && (flag != true))
	{
		int mid = (l + r) / 2; // считываем срединный индекс отрезка [l,r]

		if (str[mid].first == key)  return make_pair(true, str[mid].second); //проверяем ключ со серединным элементом
		if (str[mid].first > key) r = mid - 1; // проверяем, какую часть нужно отбросить
		else l = mid + 1;
	}
	return make_pair(flag, Base(0));
}

BigNum BigNum::Gelfond(BigNum g, BigNum a)
{
	BigNum h, b, One, i, z, q;
	h = sqrt() + Base(1);
	b = g.PowMod(h, *this);
	One += Base(1);
	i += One;
	vector<pair<BigNum, BigNum>> Gigant;
	for (; i <= h; i += One)	Gigant.push_back(make_pair(b.PowMod(i, *this), i));
	Gigant = Shall(Gigant);
	i = One;
	//for (auto j = 0; j < Gigant.size(); j++) { Gigant[j].first.Printf10(); Gigant[j].second.Printf10();  cout << endl; }
	for (; i <= h; i += One)
	{
		q = (a * g.PowMod(i, *this) % *this);
		auto  X = BitSearch(Gigant, q);
		if (X.first)	return ((h * X.second) - i) % *this;
	}
	return One;
}

BigNum BigNum::algorithmAlway(BigNum d)
{
	if (Ferma(10)) return *this;
	if (*this % Base(2) == Base(0)) throw invalid_argument("n must be odd");
	BigNum r1, r2, s, q, r;
	BigNum One, Two, Four, Zero;
	One += Base(1);
	Two = One + One;
	Four = Two + Two;
	r1 = *this % d;
	r2 = *this % (d - Two);
	s = sqrt();
	q = Four * ((*this / (d - Two)) - (*this / d));
	while (1)
	{
		/*
		d.Printf10(); cout << " ";
		r1.Printf10(); cout << " ";
		r2.Printf10(); cout << " ";
		q.Printf10();
		cout << endl;
		*/
		d += Two;
		if (d > s) { cout << "d>s" << endl; return Zero; }
		if ((Two * r1 + q) >= r2)
		{
			r = Two * r1 + q - r2;
			r2 = r1;
			r1 = r;
		}
		else
		{
			r = Two * r1 + q + d - r2;
			r2 = r1;
			r1 = r;
			q += Four;
		}
		while (r1 >= d)
		{
			r1 -= d;
			q -= Four;
		}
		if (r1 == Zero) return d;
	}

}

vector<BigNum> BigNum::Fx(vector<BigNum>str, BigNum g, BigNum a)
{
	BigNum Three, S, x, Two, One, Zero;
	vector<BigNum>tr(3);
	One += Base(1);
	Two = One + One;
	Three = Two + One;
	x = str[0];
	S = x % Three;
	
	if (S == One)
	{
		tr[0] = (a * x) % *this;
		tr[1] = str[1];
		tr[2] = (str[2] + 1) % (*this - One);
		return tr;
	}
	if (S == Two)
	{
		tr[0] = str[0].Fast_Sq() % *this;
		tr[1] = (Two * str[1]) % (*this - One);
		tr[2] = (Two * str[2]) % (*this - One);
		return tr;
	}
	else 
	{
		tr[0] = (g * x) % *this;
		tr[1] = (str[1] + 1) % (*this - One);
		tr[2] = str[2];
		return tr;
	}
}

int p_mod_1_int(int x, int n)
{
	int u1 = n;
	int u2 = 0;
	int v1 = x;
	int v2 = 1;
	while (v1 != 0)
	{
		int q = u1 / v1;
		int t1 = u1 % v1;
		int t2 = u2 - q * v2;
		u1 = v1;
		v1 = t1;
		u2 = v2;
		v2 = t2;
	}
	return (u2 % n);
}



BigNum p_mod_1(BigNum x, BigNum n)
{
	BigNum u1 = n;
	BigNum u2;
	BigNum One; One += Base(1);
	BigNum v1 = x;
	BigNum v2 = One;
	BigNum Zero;
	while (v1 != Zero)
	{
		BigNum q = u1 / v1;
		BigNum t1 = u1 % v1;
		BigNum str = (q * v2) % n;
		if (u2 < str)
		{
			u2 += n;
		}
		BigNum t2 = u2 - str;
		u1 = v1;
		v1 = t1;
		u2 = v2;
		v2 = t2;
	}
	return (u2 % n);
}

BigNum  BigNum::pPolard(BigNum g, BigNum a)
{
	BigNum One, d, r, rx, x, Zero;
	vector <BigNum> z;
	One += Base(1);
	vector <BigNum> x1(3); // x1 y1 b1;
	vector <BigNum> x2(3); // x2 y2 b2;
	x1[0] = One; x1[1] += Base(0); x1[2] += Base(0);
	x2[0] = One; x2[1] += Base(0); x2[2] += Base(0);
	while(1)
	{
		x1 = Fx(x1, g, a);
		x2 = Fx(x2, g, a);
		x2 = Fx(x2, g, a);
		if (x1[0] == x2[0]) break;
	}
	if (x1[2] < x2[2]) x1[2] += (*this - One);
	r = (x1[2] - x2[2]) % (*this - One); // 4
	if (r == Zero) return Zero;
	d = r.gcd((*this) - One); // gcd (r,n-1)
	if (x2[1] < x1[1]) x2[1] += (*this -One);
	rx = ((x2[1] - x1[1]) / d);
	r = r / d;
	x = p_mod_1(r, (*this - One) / d);
	x = (rx * x) % ((*this - One) / d);
	if (g.PowMod(x, *this) == a) return x;
	for (BigNum i = One; i < d; i += One)
	{
		x = x + (*this - 1) / d * i;
		if (g.PowMod(x, *this) == a) return x;
	}
	return Zero;
}

void pPolardTest()
{
	BigNum a, n, g;
	string d;
	while (1)
	{
		cout << "n=";
		cin >> d;
		n.Scanf10(d.c_str());
		cout << "g=";
		cin >> d;
		g.Scanf10(d.c_str());
		cout << "a=";
		cin >> d;
		a.Scanf10(d.c_str());
		cout << "Polard: ";
		n.pPolard(g, a).Printf10();
		cout << endl;
	}
	return;
}


int main()
{
	srand(time(0));
	time(nullptr);
	

	//6  pPolard lab
	pPolardTest();
	// First Lab
	/*
	BigNum a;
	a.Scanf10("99971");
	vector<BigNum> t=a.fact();
	for (int i = 0; i < t.size(); ++i) {
		t[i].Printf10();
		cout << endl;
	}

	*/

	// Second lab
	/*
	BigNum a, d;
	a.Scanf10("645");
	d.Scanf10("18");
	a.algorithmAlway(d).Printf10();
	*/

	//3 lab
	/*
	BigNum a;
	a.Scanf10("358866851");
	auto g = a.methodFermat();
	g.first.Printf10();
	cout << endl;
	g.second.Printf10();
	cout << endl;
	*/


	//4 lab
	/*
	BigNum a;
	a.Scanf10("1840312331");
	a.Polard().Printf10();
	*/


	// 5 Gelfond lab
	//
	//BigNum a, n, g;
	//n.Scanf10("1571");
	//g.Scanf10("52");
	//a.Scanf10("647");
	//n.Gelfond(g, a).Printf10();
	

}
