# Function to calculate Hamming distance
hamming_distance <- function(str1, str2) {
                # Pad the shorter string with spaces
                if (nchar(str1) < nchar(str2)) {
                                str1 <- paste0(str1, strrep(" ", nchar(str2) - nchar(str1)))
                } else if (nchar(str2) < nchar(str1)) {
                                str2 <- paste0(str2, strrep(" ", nchar(str1) - nchar(str2)))
                }
                
                # Calculate Hamming distance
                distance <- sum(strsplit(str1, NULL)[[1]] != strsplit(str2, NULL)[[1]])
                return(distance)
}

# Usernames
usernames <- list(
                c("TomiDBeloved", "The_Scholargirl"),
                c("Sheriffdeen", "Shriffdeen_H"),
                c("Sam", "Sam_001")
)

# Calculate Hamming distances for each pair of usernames
for (pair in usernames) {
                str1 <- pair[1]
                str2 <- pair[2]
                distance <- hamming_distance(str1, str2)
                print(paste("Hamming distance between", str1, "and", str2, "is:", distance))
}
