/*
** существуют 2 типа constexpr сущностей - constexpr-функции и constexpr-объекты
** e.g
*/

// пример объявления и использования constexpr-объекта
constexpr auto arr_size = 10;
std::array<int, arr_size> data;

/*
** Более интересными являются constexpr-функции, они производят константы времени компиляции когда принимают
** константы времени компиляции
** e.g.
*/

// Объявление с++11
constexpr
int
pow (int x, unsigned int y) noexcept
{
	return (y) ? x * pow (x, y - 1) : 1;
}
// Объявление с++14
constexpr
int
pow (int x, unsigned int y) noexcept
{
	int result = 1;
	while (y--)
	{
		result *= x;
	}
	return result;
}

// Вызов функции во время компиляции
constexpr int i = pow (2, 3);

int base;
int exp;
std::cin >> base;
std::cin >> exp;

// Вызов функции во время выполнения
constexpr i = pow (base, exp);

/*
** Так же constexpr функциями могут являться конструкторы классов, что позволяет создавать constexpr объекты классов и их использовать
** e.g.
*/

// c++11
class test
{
public:
	constexpr test (int x = 0, int y = 0) :
		m_x (x), m_y (y)
	{}

	void set_x (int x) noexcept {m_x = x;}
	void set_y (int y) noexcept {m_y = y;}

	constexpr int x () const noexcept {return m_x;}
	constexpr int y () const noexcept {return m_y;}
private:
	int m_x;
	int m_y;
}

// c++14
class test
{
public:
	constexpr test (int x = 0, int y = 0) :
		m_x (x), m_y (y)
	{}

	constexpr void set_x (int x) noexcept {m_x = x;}
	constexpr void set_y (int y) noexcept {m_y = y;}

	constexpr int x () noexcept const {return m_x;}
	constexpr int y () noexcept const {return m_y;}
private:
	int m_x;
	int m_y;
}
