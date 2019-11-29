#include "node.h"

/***********************************************************************
  Задача 6. Да се дефинира шаблон на функция removesub, която приема
  два параметъра first и second — указатели към линейни едносвързани
  списъци. Функцията да изтрива от списъка с начало first
  последователно всички срещания на подсписъци, които съвпадат със
  списъка с начало second. Например, при списък first с елементи 1, 2,
  3, 3, 3, 4, 5, 3, 3, 6 и second с елементи 3, 3, first да се
  преобразува до списъка 1, 2, 3, 4, 5, 6.
************************************************************************/

/***********************************************************************
 РЕШЕНИЕ:
************************************************************************/

template <typename T>
void removesub(node<T>*& first, node<T>* second){
    node<T>* secondBegin = second;
    node<T>* firstBegin = first;

    node<T>* sublistBegin = first; //node<T>* sublistBegin = first;       // In order for test case "Delete at beginning" to run
    //node<T>* sublistEnd;

    if(first == nullptr || second == nullptr){
        return;
    }

    // In order for test case "Delete at beginning" to run
    /*while (sublistBegin != nullptr && secondBegin != nullptr && sublistBegin->data == secondBegin->data){
        sublistBegin = sublistBegin->next;
        secondBegin = secondBegin->next;
    }

    if(secondBegin == nullptr){
        firstBegin = sublistBegin;
        //first = sublistBegin;    // it is better to include it.
    }*/

    while(first != nullptr){
        sublistBegin = first->next; // sublistBegin = first;
        secondBegin = second;

        while (sublistBegin != nullptr && secondBegin != nullptr && sublistBegin->data == secondBegin->data){
            sublistBegin = sublistBegin->next;
            secondBegin = secondBegin->next;
        }

        if(secondBegin == nullptr){
            if(sublistBegin != nullptr){

                first->next = sublistBegin; // first = sublistBegin->next;
                continue; //
            } else{
                first->next = nullptr; // first = nullptr;
                break; // return;
            }
        }

        if(first == nullptr){ // the program runs without this "if"
            return;
        }

        first = first->next;
    }



    /*while(first->next != nullptr){
        if(first->data == second->data){
            sublistBegin = first;
            first = first->next;
            second = second->next;

            while (first->next != nullptr && second != nullptr && first->data == second->data ){
                first = first->next;
                second = second->next;
            }

            if(first->next == nullptr && second->next == nullptr && first->data == second->data){
                first = nullptr;
                return;
            }

            if(second == nullptr){
                sublistEnd = first;
                first = sublistBegin;
                first->next = sublistEnd;
            }

        } else{
            first = first->next;
            second = secondBegin;
        }
    }*/

    first = firstBegin;
}

/***********************************************************************
 КРАЙ НА РЕШЕНИЕТО
************************************************************************/

#define DOCTEST_CONFIG_IMPLEMENT
#include "doctest.h"

/***********************************************************************
  РАЗКОМЕНТИРАЙТЕ СЛЕДВАЩИЯ РЕД, ЗА ДА ВКЛЮЧИТЕ ТЕСТОВЕТЕ
************************************************************************/
#include "prob6_tests.h"

int main ()
{
    // пускане на тестовете
    doctest::Context().run();
    return 0;
}
