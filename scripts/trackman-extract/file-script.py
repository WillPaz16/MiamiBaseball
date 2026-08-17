import os
import shutil

# Define the root directory where the 2024 folder is located
root_dir = "2024"  # Replace this with the path to your 2024 folder
destination_dir = "New"  # Replace this with the path where you want to store the month folders

# Create the month folders (01 to 07)
for month in range(1, 8):
    month_folder = os.path.join(destination_dir, f"{month:02}")
    if not os.path.exists(month_folder):
        os.makedirs(month_folder)

# Walk through the 2024 directory structure and collect CSV files
for month in range(1, 8):
    month_folder = os.path.join(root_dir, f"{month:02}")
    if os.path.exists(month_folder):
        for day in os.listdir(month_folder):
            day_folder = os.path.join(month_folder, day)
            csv_folder = os.path.join(day_folder, "CSV")
            if os.path.exists(csv_folder):
                for file in os.listdir(csv_folder):
                    if file.endswith(".csv"):
                        file_path = os.path.join(csv_folder, file)
                        # Move the CSV file to the corresponding month folder
                        shutil.move(file_path, os.path.join(destination_dir, f"{month:02}", file))

print("All CSV files have been consolidated.")
