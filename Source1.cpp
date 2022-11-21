#include <iostream>
#include <ctime>
#include <stdbool.h>
#include <string.h>
#include <type_traits> 
#include <string>
#include <bitset>
#include <cmath>
#include <chrono>

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



	BigNum& operator = (BigNum const& c)
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

	BigNum& operator = (const Base num)
	{
		delete[]Ptr;
		Len = 1;
		MaxLen = Len;
		Ptr = new Base[Len];
		Ptr[0] = num;
		return *this;
	}


	void Norm()
	{
		for (Len = MaxLen; Ptr[Len - 1] == 0 && Len > 0; Len--);
		if (Len == 0) Len = 1;
	}

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
	BigNum operator * (const BigNum&); // -
	BigNum operator+ (const Base);
	BigNum& operator+= (const Base);
	BigNum& operator*= (const Base);
	BigNum& operator*= (const BigNum&);
	BigNum operator * (const Base); // -
	BigNum operator /(const Base); // -
	BigNum& operator /=(const Base); // -
	BigNum operator /(const BigNum&); // -
	BigNum& operator /=(const BigNum&); // -
	Base operator %(const Base); // -
	BigNum operator % (const BigNum&);
	void PrintCoef();
	int SubLN(const BigNum&, int);
	void AddLN(const BigNum&, int);
	void AddMaxLen(int);
	int GetLen();
	BigNum Fast_Sq();
	BigNum pow(BigNum&);
	BigNum ShiftR(int i);
	BigNum BarretMod(BigNum&, BigNum&);
	Base bits();
	bool bit(Base);
	BigNum stupid_pow(BigNum& n)
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
	bool is_even();
	bool is_odd();
	bool Ferma(int);
	BigNum PowMod(BigNum&, BigNum&);
	bool Solovay_Strassen(int);
	BigNum SqrStr(BigNum, BigNum);
	BigNum GeneratePrime(int);
	bool MillerRab(int);
	float eps_ferma(int);
	BigNum phi();
};


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

BigNum& BigNum::operator -= (const BigNum& LN) {
	if (*this < LN) {
		throw invalid_argument("first value should be bigger than second to subtract");
	}
	int min;
	bool k = 0;
	tmp TMP;
	for (int i = 0; i < LN.Len; i++) {
		TMP = Ptr[i] + BaseMax - LN.Ptr[i] - k;
		k = !((TMP) >> BaseSize);
		Ptr[i] = (Base)TMP;
	}
	for (int i = LN.Len; k && i <= Len; i++) {
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

BigNum& BigNum::operator+= (const Base a) {
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






void BigNum::Printf10()
{
	if (Len == 1 && Ptr[0] == 0) cout << 0;
	else {
		BigNum A(*this);
		int SLen = Len * 10;
		int k = 0;
		//unsigned char tmp;
		char* S = new char[SLen];
		while (A.Ptr[0] != 0 || A.Len > 1)
		{
			S[k] = (A % 10) + '0';
			//cout<<S[k]<<' ';
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
	//PrintCoef();
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
	for (int i = Len - 1; i >= 0; i--) {
		for (int bit = BaseSize - 1; bit >= 0; bit--) {
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
	for (int i = 1; i < n_bits; i++) {
		q = q.Fast_Sq();
		if (n.bit(i)) z *= q;
	}
	return z;
}

BigNum BigNum::PowMod(BigNum& pow, BigNum& mod)
{
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
	for (size_t factor = 0; factor < new_size; factor++) {
		res.Ptr[factor] = Ptr[factor + i];
	}
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


void TestMod(int N, int M) {

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



int main()
{
	srand(time(0));
	time(nullptr);
	//TestMod(500, 500);
	//TestGen(10);
	TestPrime(10, 10, 10); // кол-во итераций, длина чисел, надежность

	/*
	BigNum a;
	a.Scanf10("40787191878995465150011");
	if (a.MillerRab(3))
	{
		cout << "Prime";
	}
	else
	{
		cout << "Not prime";
	}
	*/

	/*
	BigNum s1,n,z;
	s1.Scanf10("450");
	n.Scanf10("41");
	z = GetBarretZ(n);
	BigNum res;
	res= s1.BarretMod(n, z);
	res.Printf10();
	*/
}
