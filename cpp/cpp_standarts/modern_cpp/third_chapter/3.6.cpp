/*
** Перекрывающие функции члены должны быть объявлены как override
** e.g.
*/

class base
{
public:
	...
	virtual void test (base &);
}

// Скомпилируется без ошибок
class derived : public base
{
public:
	void test (base &) override;
}

// Выдаст ошибку компиляции
class derived : public base
{
public:
	...
	void test (base) override;
}

// Предупреждение компилятора, или немое молчание, в люобом случае неккоректная работа программы
class derived: public base
{
public:
	...
	void test (base);
}