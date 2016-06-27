#playing around with orange juice data more

oj%>% group_by(brand) %>% summarize(total_sales = sum(sales)) #dominicks, minute.maid, tropicana ordered by sales
oj%>% group_by(brand) %>% summarize(total_rev = sum(sales*price)) #minute.maid, tropicana, dominicks ordered by revenue