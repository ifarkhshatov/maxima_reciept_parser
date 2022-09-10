#install.packages("gmailr")
library(gmailr)
gm_auth_configure(path = "credentials.json")


gm_auth()

# view the latest thread
my_threads <- gm_threads(num_results = 10)

# retrieve the latest thread by retrieving the first ID

latest_thread <- gm_thread(gm_id(my_threads)[[1]])

# The messages in the thread will now be in a list
latest_thread$messages

# Retrieve parts of a specific message with the accessors
my_msg <- latest_thread$messages[[1]]

gm_to(my_msg)
gm_from(my_msg)
gm_date(my_msg)
gm_subject(my_msg)
gm_body(my_msg)
