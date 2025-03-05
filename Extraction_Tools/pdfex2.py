from PIL import Image
import glob

def create_pdf(slide_folder, output_pdf):
    images = sorted(glob.glob(f"{slide_folder}/*.png"))
    img_list = [Image.open(img).convert("RGB") for img in images]
    img_list[0].save(output_pdf, save_all=True, append_images=img_list[1:])

create_pdf("slides", "output.pdf")
