#include <map>
#include <mutex>

template <typename K, typename V>
class ThreadSafeMap {
private:
    std::map<K, V> map;
    std::mutex mutex;

public:
    void insert(const K& key, const V& value) {
        std::lock_guard<std::mutex> lock(mutex);
        map.insert(std::make_pair(key, value));
    }

    V get(const K& key) {
        std::lock_guard<std::mutex> lock(mutex);
        auto it = map.find(key);
        if (it != map.end()) {
            return it->second;
        }
        throw std::out_of_range("Key not found");
    }
};
