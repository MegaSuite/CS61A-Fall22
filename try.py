lst = [1, 5, 8, 5, 2, 3]
entry = 5
elem = 7
for id,val in enumerate(lst):
    print(id)
    if val == entry:
        print(lst)
        lst.insert(id+1,elem)
        print(lst)
# l = enumerate(lst)
# print(list(l))