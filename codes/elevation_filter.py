import pandas as pd

thres = 3000

def main(input_file, output_file, elevation_data, thres):
    elevation = pd.read_csv(elevation_data, delimiter='\t')
    elevation_filter = pd.DataFrame(elevation.loc[elevation.Elevation_Mean < thres, 'HID'])

    result = pd.read_csv(input_file)
    filtered_result = elevation_filter.merge(result, how='inner', on='HID')

    filtered_result.to_csv(output_file, index=False)

if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser()

    parser.add_argument('--input_file')
    parser.add_argument('--output_file')
    parser.add_argument('--elevation_data', required=False, default=r'./data/Spatial_Datasets/ISEA3H09_SRTM30PLUS_V11_Elevation_Mean.txt')
    parser.add_argument('--threshold', required=False, default=3000)

    args = parser.parse_args()

    main(input_file=args.input_file,
         output_file=args.output_file,
         elevation_data=args.elevation_data,
         thres=args.threshold)