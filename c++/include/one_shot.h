#include <iostream>
#include <future>
#include <mutex>

template<typename T>
class OneShot {
public:
    OneShot() : promise(), future(promise.get_future()) {
        isSet = false;
    }

    void SetValue(const T& value) {
        std::lock_guard<std::mutex> lock(mtx);
        if (isSet)
            return;
        printf("kobu\n");
        promise.set_value(value);
        printf("mugi\n");
        isSet = true;
    }

    T GetValue() {
        printf("ou hou\n");
        auto rt = future.get();
        if (rt)
            printf("hou ou\n");
        else
            printf("demon sex\n");
        return rt;
    }

private:
    std::promise<T> promise;
    std::future<T> future;
    std::mutex mtx;
    bool isSet;
};