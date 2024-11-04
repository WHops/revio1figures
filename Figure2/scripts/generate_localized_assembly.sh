#!/bin/bash

# WHoeps, Feb 2024. 
# Enhanced script for local assembly based on a BAM file and coordinates.
# It extracts reads, performs assembly with hifiasm, and converts GFA outputs to FASTA format.

# Usage: bash make_pulled_asm.sh <bam file> <coordinates> <output directory>

# Check if correct number of arguments is provided
if [ "$#" -ne 4 ]; then
    echo "Usage: $0 <bam file> <chr:start-end> <output directory> <threads>"
    exit 1
fi

# Assign input arguments to variables
bam=$1
coordinates=$2
out_dir=$3
threads=$4

echo "Starting process with BAM file: $bam"
echo "Using coordinates: $coordinates"
echo "Output will be saved in: $out_dir"

# Create output directory if it doesn't exist
mkdir -p $out_dir
if [ $? -ne 0 ]; then
    echo "Failed to create output directory: $out_dir"
    exit 1
fi

echo "Extracting reads from BAM file..."
samtools view -b $bam $coordinates > $out_dir/pulled_reads.bam
#samtools view -b $bam > $out_dir/pulled_reads.bam
if [ $? -ne 0 ]; then
    echo "Failed to extract reads from BAM file."
    exit 1
fi

echo "Converting BAM to FASTQ..."
samtools fastq $out_dir/pulled_reads.bam > $out_dir/pulled_reads.fastq
if [ $? -ne 0 ]; then
    echo "Failed to convert BAM to FASTQ."
    exit 1
fi

echo "Compressing FASTQ file..."
gzip $out_dir/pulled_reads.fastq
if [ $? -ne 0 ]; then
    echo "Failed to compress FASTQ file."
    exit 1
fi

echo "Running hifiasm for assembly..."
hifiasm -o $out_dir/pulled_reads.hifiasm $out_dir/pulled_reads.fastq.gz -t $threads -D 10 --dual-scaf
if [ $? -ne 0 ]; then
    echo "Failed to assemble with hifiasm."
    exit 1
fi

# Process both h1 and h2 GFA files
for gfa_type in "hap1" "hap2"
do
    gfa_file="$out_dir/pulled_reads.hifiasm.bp.${gfa_type}.p_ctg.gfa"
    if [ -f "$gfa_file" ]; then
        echo "Converting $gfa_file to FASTA..."
        gfatools gfa2fa $gfa_file > "${gfa_file%.gfa}.fa"
        if [ $? -ne 0 ]; then
            echo "Failed to convert GFA to FASTA for $gfa_file"
            exit 1
        fi
    else
        echo "GFA file $gfa_file not found."
    fi
done
