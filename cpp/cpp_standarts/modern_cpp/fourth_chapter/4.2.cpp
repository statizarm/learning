/*
** Необходимо использовать умные указатели типа std::shared_ptr для выражения семантики одновременного владения
** e.g.
*/

#include <memory>

// Объявление
std::shared_ptr<test> ptr;

/*
** предположим что у нас есть некоторый вектор исполняемых виджетов, и мы хотим туда добивать виджет
*/

std::vector<std::shared_ptr<Widget>> widgets;

class Widget
{
public:
	...
	void process ()
	{
		widgets.emplace_back (this); // Не верно! Может существовать другой управляющий блок вне контейнера, что приведет к ub
	}
}

// Для реализации подобного отношения необходимо использовать наследование от специализированного шаблона

class Widget : public std::enable_shared_from_this<Widget>
{
public:
	void process ()
	{
		widgets.emplace_back (shared_from_this ());
	}
}

// Для запрета создания сырых указателей на класс, можно объявить все конструкторы приватными, а создание класса производить с помощью фабричного метода

class Widget : public std::enable_shared_from_this<Widget>
{
public:
	template <typename ...agrs>
	static std::shared_ptr<Widget> create (args&&... params)
	{
		return std::make_shared<Widget> (std::forward<args> (params)...);
	}
private:
	// конструкторы
}