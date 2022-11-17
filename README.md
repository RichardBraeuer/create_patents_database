# create_patents_database
This contains code to download the PATSTAT database, organize it into files, unify spelling etc, merge the names on patent documents to institutions and feed the result to the PatentsView disambiguation algorithm to create IDs. 

The download code is in stata. The data treatment code is in its own subfolder and is in R. The debugged PatentsView code is in its own subfolder and is in Python, requiring a LINUX setup and other utilities detailed in their documentation to run.
