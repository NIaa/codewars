class Kata
{
public:
    std::vector<int> foldArray(std::vector<int> array, int runs)
    {
        for(int i = 0; i != runs; ++i) fold1(array);
        return array;
    }
private:
    std::vector<int> fold1(std::vector<int> &array)
    {   
        int sz = array.size();
        for(int i = 0; i != sz/2; ++i) array[i]+=array[sz-i-1];
        array.resize(sz/2+sz%2);
        return array;
    }
};