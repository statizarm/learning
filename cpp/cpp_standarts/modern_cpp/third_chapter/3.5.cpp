/*
** Использовоние удаленных функций вместо закрытым неопределенным
** e.g
*/

// Устаревший вариант
class test
{
public:
	...
private:
	test (const test &);
	test &operator = (const test &);
}

// современный вариант
class test
{
public:
	...
	test (const test &) = delete;
	test &operator = (const test &) = delete;
}

/*
** Помимо этого удаленные функции могут использоваться для того чтобы запретить СИ-шное приведение целочисленных типов
** e.g
*/

// Устаревший вариант
class test
{
public:
	...
	isLucky (int);
}

// Следующие вызовы являются корректными
test::isLucky ('a');
test::isLucky (true);
test::isLucky (10.8);

// Современный вариант
class test
{
public:
	...
	isLucky (int);
	isLucky (char) = delete;
	isLucky (bool) = delete;
	isLucky (double) = delete;
}

// Слудующие вызовы становятся невозможными

test::isLucky ('a');
test::isLucky (true);
test::isLucky (10.8);

/*
** Помимо этого удаленные функции делают возможным запрет использования некоторых спецификаций шаблонных функций
** e.g.
*/

// Предположим что существует объявление
template <typename T>
void
do_something (T *);

// Тогда следующие объявления удаленных функций запретят использование данных спецификаций шаблона
template <>
void
do_something <void> (void *) = delete;

template <>
void
do_something <char> (char *) = delete;
