// Person.cpp
#include <string>

class Person {
private:
    std::string m_firstName;
    std::string m_lastName;
    int m_age;
public:
    Person(std::string firstName,
           std::string lastName,
           int age) :
        m_firstName(firstName),
        m_lastName(lastName),
        m_age(age) {
    }
};
