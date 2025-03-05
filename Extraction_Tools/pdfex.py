import cv2
import numpy as np
from skimage.metrics import structural_similarity as ssim
import os

def extract_slides(video_path, output_folder, threshold=0.8):
    os.makedirs(output_folder, exist_ok=True)
    cap = cv2.VideoCapture(video_path)

    ret, prev_frame = cap.read()
    prev_gray = cv2.cvtColor(prev_frame, cv2.COLOR_BGR2GRAY)
    frame_count = 0
    slide_count = 0

    while ret:
        ret, frame = cap.read()
        if not ret:
            break
        frame_gray = cv2.cvtColor(frame, cv2.COLOR_BGR2GRAY)

        # Compute similarity using SSIM
        score, _ = ssim(prev_gray, frame_gray, full=True)
        if score < threshold:  # If significant change detected
            slide_count += 1
            cv2.imwrite(f"{output_folder}/slide_{slide_count:03d}.png", frame)
            prev_gray = frame_gray  # Update previous frame

        frame_count += 1
    cap.release()

extract_slides("video.mp4", "slides")
