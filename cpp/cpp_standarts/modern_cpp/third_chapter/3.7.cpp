/*
** const_itarator круче чем просто iterator если не надо менять элемент
** e.g.
*/

// Умеет делать все тоже самое что и обычный итератор, кроме как изменять элемент
std::vector<double>::const_iterator it;

/*
** Для максимально обобщенного кода необходимо использовать вызовы глобально определенных функций
** std::begin, std::end, std::rbegin
** e.g.
*/

std::vector<double> my_contanier;
std::vector<double>::const_iterator it = std::cbegin (my_contanier);
