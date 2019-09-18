/*
** лучше использовать std::make_unique и std::make_shared чем инициализирование соот указателей с помощью оператора new
** e.g.
*/

// shared_ptr
auto ptr = std::make_shared<test> ()
// unique_ptr
auto ptr = std::make_unique<test> ()

/*
** Но для такого подходя есть несколько проблем:
**     нельзя добавить возможно использования пользовательских удалителей
**     нельзя использовать списки инициализации
** Так же для shared_ptr этот список дополняется следующими пунктами:
**     нельзя использовать для классов использующих перегруженные операторы new и delete (т к получается несоответствие размемеров, реального блока памяти и запрашиваемого)
**     нельзя использовать для классов указатели на чьи экземпляры могут оказаться висящими, т.е. weak_ptr (память не будет освобождаться до последнего существующего объекта std::shared_ptr или std::weak_ptr)
*/
