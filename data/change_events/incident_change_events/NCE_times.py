import os
import csv
from datetime import datetime, timedelta
import xml.etree.ElementTree as ET

def custom_time_parser(timestring):
    """Custom parser to handle times beyond 24 hours."""
    hours, minutes, seconds = map(int, timestring.split(':'))
    return timedelta(hours=hours, minutes=minutes, seconds=seconds)

def format_timedelta_to_hms(td):
    """Convert a timedelta to a HH:MM:SS formatted string."""
    total_seconds = int(td.total_seconds())
    hours, remainder = divmod(total_seconds, 3600)
    minutes, seconds = divmod(remainder, 60)
    return f"{hours:02}:{minutes:02}:{seconds:02}"

path = "C:\\Users\\djarvis3\\Box\\MATSim_Analysis\\Network_Changes\\change_events"
os.chdir(path)

output_file = 'NCE_Durations.csv'

with open(output_file, 'w', newline='') as csvfile:
    csvwriter = csv.writer(csvfile)
    csvwriter.writerow(['Seed', 'Incident Link', 'Start Time [HH:MM:SS]', 'End Time [HH:MM:SS]', 'Duration [mins]'])

    for file in os.listdir():
        if file.endswith(".change_events.xml"):
            print(f"Processing file: {file}")  # Debug: print the filename
            seed = file.split('.')[0]
            tree = ET.parse(file)
            root = tree.getroot()

            # Define the namespace
            ns = {'matsim': 'http://www.matsim.org/files/dtd'}

            link_times = {}
            for networkChangeEvent in root.findall('./matsim:networkChangeEvent', ns):
                link_elem = networkChangeEvent.find('matsim:link', ns)
                if link_elem is None:  # If no 'link' element is found, skip this event.
                    continue

                link_id = link_elem.get('refId')
                start_time = custom_time_parser(networkChangeEvent.get('startTime'))
                
                if link_id in link_times:
                    link_times[link_id].append(start_time)
                else:
                    link_times[link_id] = [start_time]

            print(f"link_times for {file}: {link_times}")  # Debug: print the link_times dictionary
            
            for link, times in link_times.items():
                if len(times) == 2:  # Ensure there are two times (start and end) for the incident
                    duration = (max(times) - min(times)).total_seconds() / 60.0  # Calculate duration in minutes with decimals
                    start_str = format_timedelta_to_hms(min(times))
                    end_str = format_timedelta_to_hms(max(times))
                    csvwriter.writerow([seed, link, start_str, end_str, "{:.2f}".format(duration)])

print("Script execution complete!")
