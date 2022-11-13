#include "pch.h"
#include <utility>
#include <limits.h>
#include "fibonacci.h"

// Variables d'etat de la librairie Fibonacci
static unsigned long long previous_;  // valeur precedente
static unsigned long long current_;   // valur courante
static unsigned index_;               // position de la valeur courante

// Initialisation de la suite de Fibonacci generalisee
void fibonacci_init(
    const unsigned long long a,
    const unsigned long long b)
{
    index_ = 0;
    current_ = a;
    previous_ = b;
}

// Calcul la valeur suivante s'il n'y a pas d'overflow
bool fibonacci_next()
{
    // Check de l'overflow
    if ((ULLONG_MAX - previous_ < current_) ||
        (UINT_MAX == index_))
    {
        return false;
    }

    // Si l'indexe est 0, retourner b
    if (index_ > 0)
    {
        // Sinon, calculer le terme suivant, l'echanger et renvoyer la valeur
        previous_ += current_;
    }
    std::swap(current_, previous_);
    ++index_;
    return true;
}

// Renvoie la valeur courante de la serie
unsigned long long fibonacci_current()
{
    return current_;
}

// Renvoie l'indexe courant de la serie
unsigned fibonacci_index()
{
    return index_;
}